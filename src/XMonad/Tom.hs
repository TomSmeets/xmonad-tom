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
import XMonad.Hooks.DynamicLog          
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.PerWorkspace        
import XMonad.Layout.SimplestFloat
import XMonad.StackSet                  (allWindows, greedyView, shift)
import XMonad.Util.EZConfig             (additionalKeysP, additionalKeys)
import XMonad.Util.Run                  (runProcessWithInput)

import XMonad.Tom.UI.Dialog

import qualified XMonad.Tom.XMobar as B
import qualified XMonad.Tom.XMobarHs as BU

import qualified XMonad.Tom.Workspace as W
import qualified XMonad.Tom.Workspace.Select as WS
import qualified XMonad.Tom.Workspace.History as WSH

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
-- |
-- Helper function which provides ToggleStruts keybinding
--
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

-- Extensible layouts
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
modes = tiled ||| Mirror tiled ||| simplestFloat ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

myLayout = onWorkspace "Float" simplestFloat
         . avoidStruts  -- Don't cover the statusbar
         . smartBorders -- Don't show borders when in fullscreen
         $ modes

myConfig = defaultConfig { terminal          = "gnome-terminal"
                         , manageHook        = manageSpawn <+> manageDocks -- <+> mkWSHook myWorkspaces
                         , workspaces        = map (show . W.index) $ flatten W.myTree --map label myWorkspaces
                         , startupHook       = onFirst (doWSSpawns) >> startup
                         , layoutHook        = myLayout
                         , logHook           = dynamicLogWithPP $ defaultPP
                         , focusFollowsMouse = False
                         }
    `additionalKeysP` [ ("M-p",   spawn "j4-dmenu-desktop")                                                -- dmenu for .desktop files

                      , ("M-f",   spawn "amixer set Capture toggle")
                      , ("M-S-f", doBG)

                      , ("M-r",   WS.runWS False) -- Go to workspace with treeselect
                      , ("M-m",   WS.runWS True)  -- Move and go to workspace with treeselect

                      , ("M-o",   WSH.doUndo)
                      , ("M-i",   WSH.doRedo)

                      , ("M-q", spawn "gnome-terminal --working-directory $HOME/Programming/Haskell/Doing/xmonad-tom -x stack install && xmonad --restart")
                      , ("M-S-q", void . runDialogX $ Node (Choice "root" (return ())) [ Node (Choice "logout"   $ closeAll >> io exitSuccess)               []
                                                                                       , Node (Choice "restart"  $ closeAll >> spawn "sudo shutdown now -r") []
                                                                                       , Node (Choice "shutdown" $ closeAll >> spawn "sudo shutdown now")    []
                                                                                       ])
                      ]
    `additionalKeys`  [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
                      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
                      , ((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
                      ]


doWSSpawns :: X ()
doWSSpawns = do
    spawnOn (findIdx "Browser")     "iceweasel"
    spawnOn (findIdx "Programming") "gnome-terminal --working-directory $HOME/Programming"
  where
    findIdx name = show . W.index . label . fromJust . W.searchBelow ((==name) . W.name) . fromTree $ W.myTree

myGSConfig :: GSConfig WorkspaceId
myGSConfig = defaultGSConfig

-- | Startup hook
startup :: X ()
startup = do
    io (BU.export B.config) -- apply xmobar file
    setWMName "LG3D" -- to make java swing work
    doBG

doBG = spawn "FractalArt && feh --bg-fill $HOME/.fractalart/wallpaper.bmp"

showBG n = spawn $ "feh --bg-fill " ++ bgPath n
mkBG   n = spawn $ "FractalArt -f " ++ bgPath n
bgPath n = "$HOME/.fractalart/" ++ show n ++ ".bmp"

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

