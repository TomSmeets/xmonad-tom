module XMonad.Tom.UI.Dialog ( runDialog
                            , runDialogX
                            , Choice(..)
                            ) where

import XMonad.Util.NamedWindows (getName)
import Control.Monad                    (join, when)
import Control.Monad.State              (evalStateT,  get,  gets, StateT)
import Control.Monad.Trans              (lift)
import Data.Function                    (fix)
import Data.Maybe                       (fromMaybe)
import Data.List                        (find)
import Data.Tree.Zipper                 (fromTree, toTree, label, next, prev, parent)
import Data.Tree                        (Tree(..))
import XMonad                    hiding (gets, get)
import XMonad.Actions.GridSelect        (shadowWithKeymap)
import XMonad.Layout.Decoration         (shrinkIt,  shrinkText, shrinkWhile) 
import XMonad.Prompt                    (mkUnmanagedWindow)
import XMonad.Tom.Workspace             (Workspace(..),  ZTree,  doAll,  myTree,  searchBelow,  setFold, unfoldParents, child)
import XMonad.Tom.Workspace.History
import XMonad.Util.Font                 (initXMF,  printStringXMF,  releaseXMF,  textWidthXMF, XMonadFont)

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

data WSelect a = WSelect { wsTree   :: ZTree (Choice a)
                         , wsScreen :: (Integer, Integer)
                         , wsWindow :: Window
                         , wsFont   :: XMonadFont
                         }

type WS a = StateT (WSelect a) X

data Choice a = Choice { cName :: String, cValue :: a }

runDialogX :: Tree (Choice (X a)) -> X (Maybe a)
runDialogX t = do
    v <- runDialog t
    case v of
        Just x  -> Just <$> x
        Nothing -> return Nothing

runDialog :: Tree (Choice a) -> X (Maybe a)
runDialog tree = do
    let initTree = fromTree tree
    me  <- withWindowSet (return . W.tag . W.workspace . W.current)
    mkWindow "xft:Sans-16" initTree

mkWindow :: String -> ZTree (Choice a) -> X (Maybe a)
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

myNavigation :: WS a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where 
    navKeyMap = M.fromList [ ((0, xK_Escape), return Nothing)
                           , ((0, xK_Return), gets (Just . cValue . label . wsTree))
                           , ((0, xK_j),      moveTree next   False >> myNavigation)
                           , ((0, xK_k),      moveTree prev   False >> myNavigation)
                           , ((0, xK_l),      moveTree child  False >> myNavigation)
                           , ((0, xK_h),      moveTree parent False >> myNavigation)
                           ]
    navDefaultHandler = const myNavigation

-- | Evaluate the 'WS' monad
evalWS :: WS a b -> WSelect a -> X b
evalWS = evalStateT

-- | Apply a transformation, if it succeeds then redraw
moveTree :: (ZTree (Choice a) -> Maybe (ZTree (Choice a))) -> Bool -> WS a ()
moveTree f full = do
    w <- get
    -- only redraw if the tree has been changed
    case f $ wsTree w of
        Just t  -> put (w{wsTree = t}) >> redraw full
        Nothing -> return () 

-- | Same as 'moveTree' but dont redraw the screen
moveTree' :: (ZTree (Choice a) -> Maybe (ZTree (Choice a))) -> WS a ()
moveTree' f = modify (\w -> w{wsTree = fromMaybe (wsTree w) (f (wsTree w))})

-- | Redraw the screen
redraw :: Bool -> WS a ()
redraw full = do 
    win <- gets wsWindow
    fnt <- gets wsFont
    when full $ lift $ withDisplay (\dpy -> liftIO $ do
        clearWindow dpy win
        )
    -- moveTree' (Just . unfoldParents . doAll (setFold True))
    indents <- gets (toIndents . toTree . wsTree)
    current <- gets (label . wsTree)
    sequence_ $ zipWith (\(w, x) y -> drawNode x y w (cName w == cName current) win fnt) indents [0..]
    return ()

-- | Convert a Tree to a list with an indent level
toIndents :: Tree a -> [(a, Integer)]
toIndents (Node w ws) = (w, 0) : concatMap (map inc . toIndents) ws
  where
    inc (x, i) = (x, i+1)

-- | Draw a node at a given indentation level and y level
drawNode :: Integer -> Integer -> Choice a -> Bool -> Window -> XMonadFont -> WS a ()
drawNode ix iy ws hl win fnt = do
    let tab   = 80
        ox    = 0
        oy    = 0
        nodeW = 200
        nodeH = 30

    let x = ix * tab + ox
        y = iy * nodeH + oy
    
    (scW, scH) <- gets wsScreen
    
    let col = (cycle colorsInactive !! fromIntegral iy, "black")

    lift $ drawWinBox win fnt (highlighted hl col) nodeH nodeW (cName ws) x y 8

highlighted True  = const ("#FF0000", "white")
highlighted False = id

-- | A simple color-palette to choose from for each node
colorsActive, colorsInactive :: [String]
colorsActive   = colorsInactive -- ["#9FCE10", "#BEF249"]
colorsInactive = ["#10B8D6", "#50D0DB"]
colorBG        = 0xFFFFFF
colorBGStr     = "#FFFFFF"

-- | create an event handler, blocks until one event has been handled, you have to do the looping yourself
makeXEventhandler :: ((KeySym, String, KeyMask) -> WS a (Maybe b)) -> WS a (Maybe b)
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
