{-# LANGUAGE LambdaCase #-}
module XMonad.Tom where

import Control.Concurrent               (threadDelay)
import Control.Monad
import Data.Maybe
import Data.Tree (Tree(..), flatten)
import Data.Tree.Zipper
import Graphics.X11.ExtraTypes.XF86
import System.Exit                      (exitSuccess)
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn           (spawnOn, manageSpawn)
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen         (fullscreenSupport)
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.StackSet                  hiding (workspaces)
import XMonad.Util.EZConfig             (additionalKeysP, additionalKeys)
import XMonad.Util.Run                  (runProcessWithInput)

import XMonad.Tom.UI.Dialog
import qualified XMonad.Tom.Workspace         as W
import qualified XMonad.Tom.Workspace.History as WSH
import qualified XMonad.Tom.Workspace.Select  as WS
import qualified XMonad.Tom.XMobar            as B
import qualified XMonad.Tom.XMobarHs          as BU

main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

myPP = defaultPP { ppCurrent         = const ""
                 , ppVisible         = const ""
                 , ppHidden          = const ""
                 , ppHiddenNoWindows = const ""
                 , ppUrgent          = const ""
                 , ppSep             = ":"
                 , ppWsSep           = ""
                 , ppTitle           = id -- shorten 80
                 , ppLayout          = const ""
                 , ppOrder           = id
                 , ppExtras          = []
                 }

-- | Helper function which provides ToggleStruts keybinding
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
modes = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

myLayout = avoidStruts  -- Don't cover the statusbar
         . smartBorders -- Don't show borders when in fullscreen
         $ modes

myConfig = fullscreenSupport $ defaultConfig 
    { terminal          = "urxvt"
    , manageHook        = manageSpawn <+> manageDocks
    , workspaces        = map (show . W.index) $ flatten W.myTree --map label myWorkspaces
    , startupHook       = onFirst doWSSpawns >> startup
    , layoutHook        = myLayout
    , logHook           = dynamicLogWithPP $ defaultPP
    , focusFollowsMouse = False
    , borderWidth       = 2
    }
    `additionalKeysP` [ -- dmenu to lauch commands, j4-dmenu to lauch .desktop files
                        ("M-p",   spawn "j4-dmenu-desktop")
                      , ("M-S-p", spawn "dmenu_run")

                      -- media controls
                      , ("M-f",   spawn "amixer set Capture toggle")
                      , ("M-S-f", spawn "amixer set Master toggle")

                      -- movement
                      , ("M-r",   WS.runWS False) -- Go to workspace with treeselect
                      , ("M-S-r", WS.runWS True)  -- Move and go to workspace with treeselect
                      , ("M-o",   WSH.doUndo)
                      , ("M-i",   WSH.doRedo)

                      -- util actions
                      , ("M-u", void . runDialogX $ listToTree (Choice "" $ return ())
                            [ Choice "Wallpaper" $ doBG
                            ])

                      -- xmonad actions
                      , ("M-q", void . runDialogX $ listToTree (Choice "" $ return ())
                            [ Choice "compile" $ spawn "gnome-terminal --working-directory $HOME/Programming/Haskell/Done/xmonad-tom -x ./XMBuild"
                            , Choice "restart" $ spawn "xmonad --restart"
                            , Choice "xmobar"  $ io (BU.export B.config >> spawn "pkill xmobar; xmonad --restart")
                            ])

                      -- system actions
                      , ("M-S-q", void . runDialogX $ listToTree (Choice "" $ return ())
                            [ Choice "logout"   $ closeAll >> io exitSuccess
                            , Choice "restart"  $ closeAll >> spawn "sudo shutdown now -r"
                            , Choice "shutdown" $ closeAll >> spawn "sudo shutdown now"
                            ])
                      , ("M-S-e", windows (\set -> set { current = swap (current set) (head $ visible set)
                                                       , visible = swap (head (visible set)) (current set) : tail (visible set)}))
                      ]
    `additionalKeys`  [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2.5%-")
                      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2.5%+")
                      , ((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
                      ]

swap (Screen ws i d) (Screen ws' i' d') = Screen ws' i d
myManage = isFullscreen --> doFullFloat

listToTree :: a -> [a] -> Tree a
listToTree r = Node r . map (`Node` [])

doWSSpawns :: X ()
doWSSpawns = do
    spawnOn (findIdx "Browser")     "iceweasel"
  where
    findIdx name = show . W.index . label . fromJust . W.searchBelow ((==name) . W.name) . fromTree $ W.myTree

-- | Startup hook
startup :: X ()
startup = do
    setWMName "LG3D" -- to make java swing work
    doBG

-- | Generate next wallpaper
doBG :: X ()
doBG = spawn "FractalArt -w 1920 -h 1200 -f $HOME/.fractalart/wallpaperL.bmp\
           \& FractalArt -w 1920 -h 1080 -f $HOME/.fractalart/wallpaperR.bmp & wait \
           \&& feh --bg-fill $HOME/.fractalart/wallpaperL.bmp --bg-fill $HOME/.fractalart/wallpaperR.bmp"

-------------------------------------------

-- | Run only on first startup
onFirst :: X () -> X ()
onFirst m = windowCount >>= \n -> unless (n > 0) m

-- | Prepare for shutdown, wait for Dropbox and close all windows
closeAll :: X ()
closeAll = waitDropbox >> closeWindows >> sleep 1

-- | Close all open windows in all workspaces
closeWindows :: X ()
closeWindows = withWindowSet (mapM_ killWindow . allWindows)

-- | Wait for Dropbox to finish syncing
waitDropbox :: X Bool
waitDropbox = runProcessWithInput "dropbox" ["status"] "" >>= \case
        "Up to date\n"             -> return True        -- Up to date, dropbox is done syncing
        "Dropbox isn't running!\n" -> return False       -- Dropbox isn't running, no need to wait
        _ -> io (threadDelay $ seconds 3) >> waitDropbox -- Otherwise wait 3 seconds and try again

-- | sleep t seconds
sleep :: Float -> X ()
sleep = io . threadDelay . seconds

-- | convert seconds to microsecond
seconds :: Float -> Int
seconds n = floor $ n * 10**6

-- | Get the number of open windows
windowCount :: X Int
windowCount = withWindowSet (return . length . allWindows)

