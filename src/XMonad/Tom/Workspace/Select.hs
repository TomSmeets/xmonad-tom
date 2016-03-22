module XMonad.Tom.Workspace.Select where

import XMonad.Util.NamedWindows (getName)
import Control.Monad                    (join, when)
import Control.Monad.State              (evalStateT,  get,  gets, StateT)
import Control.Monad.Trans              (lift)
import Data.Function                    (fix)
import Data.Maybe                       (fromMaybe)
import Data.List                        (find)
import Data.Tree.Zipper                 (fromTree, toTree, label, next, prev, parent)
import Data.Tree
import XMonad                    hiding (gets, get)
import XMonad.Actions.GridSelect        (shadowWithKeymap)
import XMonad.Layout.Decoration         (shrinkIt,  shrinkText, shrinkWhile) 
import XMonad.Prompt                    (mkUnmanagedWindow)
import XMonad.Tom.Workspace             (Workspace(..),  ZTree,  doAll, searchBelow,  setFold,  toIndents,  unfoldParents, child)
import XMonad.Tom.Workspace.History
import XMonad.Util.Font                 (initXMF,  printStringXMF,  releaseXMF,  textWidthXMF, XMonadFont)

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

data WSelect = WSelect { wsTree   :: ZTree Workspace
                       , wsScreen :: (Integer, Integer)
                       , wsWindow :: Window
                       , wsFont   :: XMonadFont
                       }

type WS = StateT WSelect X

runWS :: Tree Workspace -> Bool -> X ()
runWS t shift = do
    let initTree = fromTree t
    me <- withWindowSet (return . W.tag . W.workspace . W.current)
    let findMe = searchBelow (\w -> path w == me) 

    idx <- mkWindow "xft:Sans-16" (fromMaybe initTree (unfoldParents <$> findMe initTree))
    case idx of 
        Just i -> do
                if shift then moveToWS i
                         else goToWS i
        _      -> return ()

goToSelected :: WS ()
goToSelected = gets (path . label . wsTree) >>= lift . goToWS

findWS p = withWindowSet (\winset -> return . find (\ws -> p == W.tag ws) . W.workspaces $ winset)

mkWindow :: String -> ZTree Workspace -> X (Maybe String)
mkWindow fnt t = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    s     <- gets $ screenRect . W.screenDetail . W.current . windowset
    win   <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)
    liftIO $ mapWindow dpy win
    liftIO $ setWindowBackground dpy win colorBG
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    io $ grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
    font <- initXMF fnt
    let screenWidth  = toInteger $ rect_width  s;
        screenHeight = toInteger $ rect_height s;
    txt <- if status == grabSuccess then evalWS (redraw True >> myNavigation) $ WSelect { wsTree   = t
                                                                                   , wsScreen = (screenWidth, screenHeight)
                                                                                   , wsWindow = win
                                                                                   , wsFont   = font
                                                                                   }
                                    else return Nothing
    liftIO $ do
        unmapWindow dpy win
        destroyWindow dpy win
        sync dpy False

    releaseXMF font
    return txt

myNavigation :: WS (Maybe String)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where 
    navKeyMap = M.fromList [ ((0, xK_Escape), return Nothing)
                           , ((0, xK_Return), gets (Just . path . label . wsTree))
                           , ((0, xK_Up),    up)
                           , ((0, xK_Down),  down)
                           , ((0, xK_Left),  left)
                           , ((0, xK_Right), right)
                           , ((0, xK_k), up)
                           , ((0, xK_j), down)
                           , ((0, xK_h), left)
                           , ((0, xK_l), right)
                           ]
    down  = moveTree next False  >> myNavigation
    up    = moveTree prev False  >> myNavigation
    right = moveTree (\w -> (unfoldParents . doAll (setFold True)) <$> child w) True >> myNavigation
    left  = moveTree parent True >> myNavigation
    navDefaultHandler = const myNavigation

-- | Evaluate the 'WS' monad
evalWS :: WS a -> WSelect -> X a
evalWS = evalStateT

-- | Apply a transformation, if it succeeds then redraw
moveTree :: (ZTree Workspace -> Maybe (ZTree Workspace)) -> Bool -> WS ()
moveTree f full = do
    w <- get
    -- only redraw if the tree has been changed
    case f $ wsTree w of
        Just t  -> put (w{wsTree = t}) >> redraw full
        Nothing -> return () 

-- | Same as 'moveTree' but dont redraw the screen
moveTree' :: (ZTree Workspace -> Maybe (ZTree Workspace)) -> WS ()
moveTree' f = modify (\w -> w{wsTree = fromMaybe (wsTree w) (f (wsTree w))})

-- | Redraw the screen
redraw :: Bool -> WS ()
redraw full = do 
    win <- gets wsWindow
    fnt <- gets wsFont
    when full $ lift $ withDisplay (\dpy -> liftIO $ do
        clearWindow dpy win
        )
    moveTree' (Just . unfoldParents . doAll (setFold True))
    indents <- gets (toIndents . toTree . wsTree)
    current <- gets (label . wsTree)
    sequence_ $ zipWith (\(w, x) y -> drawNode x y w (path w == path current) win fnt) indents [0..]
    return ()


-- | Draw a node at a given indentation level and y level
drawNode :: Integer -> Integer -> Workspace -> Bool -> Window -> XMonadFont -> WS ()
drawNode ix iy ws hl win fnt = do
    let tab   = 80
        ox    = 0
        oy    = 0
        nodeW = 200
        nodeH = 30

    let x = ix * tab + ox
        y = iy * nodeH + oy
    
    (scW, scH) <- gets wsScreen
    
    wws <- lift . findWS . path $ ws
    let windowInWS = (W.focus <$> (wws >>= W.stack))
    mes <- lift $ fromMaybe (return "") $ (fmap show . getName) <$> windowInWS
    -- mes <- lift $ (maybe "" (getName . W.focus) . W.stack) <$> (findWS $ index ws)
    let (col, col2) = case windowInWS of
                        Just _   -> ( (cycle colorsActive !! fromIntegral iy, "black")
                                    , highlighted hl (colorBGStr, "black")
                                    )
                        Nothing  -> ( (cycle colorsInactive !! fromIntegral iy, "black")
                                    , (colorBGStr, "black")
                                    )

    lift $ drawWinBox win fnt (highlighted hl col) nodeH nodeW (name ws) x y 8
    lift $ drawWinBox win fnt col2 nodeH (scW-x) (mes) (x+nodeW) y 8

highlighted True  = const ("#FF0000", "white")
highlighted False = id

-- | A simple color-palette to choose from for each node
colorsActive, colorsInactive :: [String]
colorsActive   = colorsInactive -- ["#9FCE10", "#BEF249"]
colorsInactive = ["#10B8D6", "#50D0DB"]
colorBG        = 0xD0D0D0
colorBGStr     = "#D0D0D0"

-- | create an event handler, blocks until one event has been handled, you have to do the looping yourself
makeXEventhandler :: ((KeySym, String, KeyMask) -> WS (Maybe a)) -> WS (Maybe a)
makeXEventhandler keyhandler = fix $ \_ -> join $ lift $ withDisplay $ \d -> liftIO $ allocaXEvent $ \e -> do
                             maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e
                             ev <- getEvent e
                             if ev_event_type ev == keyPress
                               then do
                                  (ks,s) <- lookupString $ asKeyEvent e
                                  return $ do
                                      mask <- lift $ cleanMask (ev_state ev)
                                      keyhandler (fromMaybe xK_VoidSymbol ks, s, mask)
                               else
                                  return $ return Nothing

-- | draw a simple box with text on a window
drawWinBox :: Window           -- ^ window to draw on
           -> XMonadFont       -- ^ font too use
           -> (String, String) -- ^ background and forground color, somehow the background color is the text and the forground color is the background
           -> Integer          -- ^ width of the box
           -> Integer          -- ^ height of the box
           -> String           -- ^ text do draw inside the box 
           -> Integer          -- ^ x position of the box (distance from left)
           -> Integer          -- ^ y position of the box (distance from top)
           -> Integer          -- ^ text spacing from the border
           -> X ()
drawWinBox win font (fg,bg) ch cw text x y cp =
    withDisplay $ \dpy -> do
    gc <- liftIO $ createGC dpy win
    bordergc <- liftIO $ createGC dpy win
    liftIO $ do
        Just fgcolor <- initColor dpy fg
        Just bgcolor <- initColor dpy bg
        -- Just bordercolor <- initColor dpy "black"
        let bordercolor = fgcolor
        setForeground dpy gc fgcolor
        setBackground dpy gc bgcolor
        setForeground dpy bordergc bordercolor
        fillRectangle dpy win gc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
        drawRectangle dpy win bordergc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
    stext <- shrinkWhile (shrinkIt shrinkText) (\n -> do 
        size <- liftIO $ textWidthXMF dpy font n
        return $ size > (fromInteger (cw-(2*cp)))
        ) text
    printStringXMF dpy win font gc bg fg (fromInteger (x+cp)) (fromInteger (y+(div ch 2)+cp)) stext
    liftIO $ do 
        freeGC dpy gc
        freeGC dpy bordergc
