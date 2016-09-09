{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.Tom where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent               (threadDelay)
import Control.Monad
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import System.Exit                      (exitSuccess)
import XMonad
import XMonad.Actions.SpawnOn           (spawnOn, manageSpawn)
import XMonad.Actions.TreeSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Fullscreen         (fullscreenSupport)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.StackSet                  as W hiding (workspaces)
import XMonad.Util.EZConfig             (additionalKeysP, additionalKeys)
import XMonad.Util.Run                  (runProcessWithInput)
import qualified XMonad.Hooks.ICCCMFocus as ICCCMFocus

import qualified XMonad.Tom.XMobarHs          as BU
import qualified Data.Map as M

-- | This PP only shows the current title of the focused Window.
titlePP = defaultPP { ppCurrent         = hlLast
                    , ppVisible         = const ""
                    , ppHidden          = const ""
                    , ppHiddenNoWindows = const ""
                    , ppUrgent          = const ""
                    , ppSep             = xmobarColor "white" "" " | "
                    , ppWsSep           = ""
                    , ppTitle           = id
                    , ppLayout          = id -- const ""
                    , ppOrder           = \[w, l, t] -> [l, w, t]
                    , ppExtras          = []
                    }
  where
      hlLast path = let (h, t) = span (/= '.') $ reverse path
                     in xmobarColor "white" "" (reverse t) ++ xmobarColor "green" "" (reverse h)

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

myConfig = ewmh $ fullscreenSupport $ myKeys $ defaultConfig
    { terminal          = "urxvt"
    , manageHook        = manageSpawn <+> manageDocks
    , handleEventHook   = docksEventHook
    , layoutHook        = myLayout
    , logHook           = dynamicLogWithPP defaultPP <+> ICCCMFocus.takeTopFocus <+> workspaceHistoryHook
    , focusFollowsMouse = False
    , borderWidth       = 2
    , modMask           = mod1Mask
    }

withWSTree t conf = conf { workspaces = toWorkspaces t }
    `additionalKeysP` [ ("M-r",   treeselectWorkspace def t W.greedyView) -- Go to workspace with treeselect
                      , ("M-S-r", treeselectWorkspace def t (\i -> W.greedyView i . W.shift i))  -- Move and go to workspace with treeselect
                      ]

screenID :: X ScreenId
screenID = withWindowSet (return . screen . current)

myKeys conf = conf `additionalKeysP` [ -- dmenu to lauch commands, j4-dmenu to lauch .desktop files
           ("M-p",   screenID >>= \(S i) -> spawn $ "j4-dmenu-desktop --dmenu='dmenu -i -p .desktop -m " ++ show i ++ "'")
         , ("M-S-p", screenID >>= \(S i) -> spawn $ "dmenu_run -i -p cmd -m " ++ show i)

         -- media controls
         , ("M-f",   spawn "amixer set Capture toggle")
         , ("M-S-f", spawn "amixer set Master toggle")
         , ("M-S-l", spawn "slock")

         -- xmonad actions
         , ("M-S-q", do n <- windowCount
                        let msg = if n == 0 then "(No open windows)"
                                            else "( " ++ show n ++ " open windows)"
                        treeselectAction def [ Node (TSNode "Shutdown" ("Poweroff my computer " ++ msg) (closeWindows >> spawn "poweroff")) []
                                             , Node (TSNode "Restart"  "Restart my computer"            (closeWindows >> spawn "reboot"))   []
                                             , Node (TSNode "Logout"   "Kill XMonad"                    (closeWindows >> io exitSuccess))   []
                                             ])
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

mkXMobarConf = BU.export

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

onStartup m conf = conf { startupHook = onFirst $ startupHook conf >> m }

-------------------------------------------

-- | Run only on first startup
onFirst :: X () -> X ()
onFirst m = windowCount >>= \n -> unless (n > 0) m

-- | Close all open windows in all workspaces
closeWindows :: X ()
closeWindows = do
    withWindowSet (mapM_ killWindow . allWindows)
    waitDropbox
    -- wait for all the windows to close
    -- Does this actually work?
    withDisplay (\d -> io $ sync d False)

-- | Wait for Dropbox to finish syncing
waitDropbox :: X Bool
waitDropbox = runProcessWithInput "dropbox" ["status"] "" >>= \case
        "Up to date\n"             -> return True        -- Up to date, dropbox is done syncing
        "Dropbox isn't running!\n" -> return False       -- Dropbox isn't running, no need to wait
        _ -> io (threadDelay $ seconds 3) >> waitDropbox -- Otherwise wait 3 seconds and try again

-- | convert seconds to microsecond
seconds :: Float -> Int
seconds n = floor $ n * 10**6

-- | Get the number of open windows
windowCount :: X Int
windowCount = gets (length . allWindows . windowset)
