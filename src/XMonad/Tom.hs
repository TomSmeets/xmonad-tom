{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.Tom where

import Control.Concurrent               (threadDelay)
import Control.Monad
import Data.Maybe
import Data.Tree (Tree(..), flatten)
import Data.Tree.Zipper
import Graphics.X11.ExtraTypes.XF86
import System.Exit                      (exitSuccess)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn           (spawnOn, manageSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen         (fullscreenSupport)
import XMonad.Layout.LayoutModifier
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

-- | This PP only shows the current title of the focused Window.
titlePP = defaultPP { ppCurrent         = const ""
                    , ppVisible         = const ""
                    , ppHidden          = const ""
                    , ppHiddenNoWindows = const ""
                    , ppUrgent          = const ""
                    , ppSep             = ":"
                    , ppWsSep           = ""
                    , ppTitle           = id
                    , ppLayout          = const ""
                    , ppOrder           = id
                    , ppExtras          = []
                    }

myLayout = avoidStruts  -- Don't cover the statusbar
         . smartBorders -- Don't show borders when in fullscreen
         $ modes

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

myConfig = fullscreenSupport $ myKeys $ defaultConfig 
    { terminal          = "urxvt"
    , manageHook        = manageSpawn <+> manageDocks
-- , workspaces        = map W.path $ flatten W.myTree --map label myWorkspaces
    , layoutHook        = myLayout
    , logHook           = dynamicLogWithPP $ defaultPP
    , focusFollowsMouse = False
    , borderWidth       = 2
    }

-- TODO: set names as path like root.programming.1
withWSTree t conf = conf { workspaces = map W.path $ flatten t } `additionalKeysP` [ ("M-r",   WS.runWS t False) -- Go to workspace with treeselect
                                                                                   , ("M-S-r", WS.runWS t True)  -- Move and go to workspace with treeselect
                                                                                   , ("M-o",   WSH.doUndo)  -- Go back to the last Workspace
                                                                                   , ("M-i",   WSH.doRedo)  -- Go foreward in your undo history
                                                                                   ]

myKeys conf = conf `additionalKeysP` [ -- dmenu to lauch commands, j4-dmenu to lauch .desktop files
           ("M-p",   spawn "j4-dmenu-desktop")
         , ("M-S-p", spawn "dmenu_run")

         -- media controls
         , ("M-f",   spawn "amixer set Capture toggle")
         , ("M-S-f", spawn "amixer set Master toggle")

         -- movement

         -- Display a list of actions

         -- xmonad actions
         , ("M-q", void . runDialogX $ listToTree (Choice "" $ return ())
               [ Choice "recompile" $ spawn "gnome-terminal --working-directory $HOME/Programming/Haskell/Done/xmonad-tom -x ./XMBuild"
               , Choice "restart" $ spawn "xmonad --restart"
               , Choice "xmobar"  $ io (BU.export B.config >> spawn "pkill xmobar; xmonad --restart")
               ])

         -- system actions
         , ("M-S-q", void . runDialogX $ listToTree (Choice "" $ return ())
               [ Choice "shutdown" $ closeAll >> spawn "sudo shutdown now"
               , Choice "restart"  $ closeAll >> spawn "sudo shutdown now -r"
               , Choice "logout"   $ closeAll >> io exitSuccess
               ])
         -- swap workspaces with screens
         ]
    `additionalKeys`  [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2.5%-")
                      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2.5%+")
                      , ((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
                      ]

listToTree :: a -> [a] -> Tree a
listToTree r = Node r . map (`Node` [])

-- | enable XMobar, to generate a config use mkXMobarConf xmconf
-- M-b: toggle xmobar
withXMobar :: LayoutClass l Window => PP -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
withXMobar pp conf = statusBar "xmobar" pp (const (modMask conf, xK_b)) conf

mkXMobarConf xmconf = BU.export xmconf

-- | Apply dualScreen support
-- M-S-e to swap screens
dualScreen conf = conf `additionalKeysP` 
    [ -- swap the workspaces on screens 
      ("M-S-e", windows (\set -> set { current = swap (current set) (head $ visible set)
                                     , visible = swap (head (visible set)) (current set) : tail (visible set)}))
    ]
  where
      swap (Screen ws i d) (Screen ws' i' d') = Screen ws' i d

fixJava conf = conf { startupHook = setWMName "LG3D" >> startupHook conf }

runInWS cmd ws conf = onStartup (spawnOn ws cmd) $
    conf { manageHook = manageHook conf <+> composeAll [ className =? cmd --> doShift ws
                                                       , title     =? cmd --> doShift ws
                                                       , appName   =? cmd --> doShift ws
                                                       ]
         }

onStartup m conf = conf { startupHook = startupHook conf >> m }

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

