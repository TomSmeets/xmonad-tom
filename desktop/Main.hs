module Main where

import Control.Monad
import Data.List
import Data.Tree
import Data.Tree.Zipper
import XMonad
import XMonad.Tom
import XMonad.Tom.UI.Dialog
import XMonad.Tom.Workspace
import XMonad.Tom.XMobarHs as XMobar
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< withXMobar titlePP conf

conf = runInWS "iceweasel" "root.Left.Browser"
     . onStartup doBG
     . dualScreen
     . fixJava
     . withWSTree myTree
     . flip additionalKeysP [ ("M-u", void . runDialogX $ listToTree (Choice "" $ return ())
        [ Choice "Wallpaper" doBG
        , Choice "XMobar" (io $ (export xmobarconf >> spawn "pkill xmobar; xmonad --restart"))
        ])]
     $ myConfig

doBG :: X ()
doBG = spawn "FractalArt -w 1920 -h 1200 -f $HOME/.fractalart/wallpaperL.bmp\
           \& FractalArt -w 1920 -h 1080 -f $HOME/.fractalart/wallpaperR.bmp & wait \
           \&& feh --bg-fill $HOME/.fractalart/wallpaperL.bmp --bg-fill $HOME/.fractalart/wallpaperR.bmp"

myTree :: Tree Workspace
myTree = idx $ nd "root"
           [ nd "Left"
               [ nd "Browser"     []
               , nd "Programming" $ numbers 0
               , nd "Home" $
                   [ nd "Aptitude" []
                   ] ++ numbers 0
               , nd "Game" $ numbers 0
               ]
           , nd "Right"
               [ nd "Docs" $ numbers 0
               , nd "Youtube" []
               , nd "Skype" []
               , nd "Steam" []
               , nd "Telegram" []
               ]
           ]
  where
    nd x xs = Node (Workspace "" x True) xs
    numbers n = map (\c -> nd [c] []) . drop n $ ['a'..'f']
    idx = fmap (\(w, p) -> w{ path = intercalate "." p }) . treePath name


xmobarconf = XMobar.defaultConfig
   { font            = "xft:monospace:size=10:bold"
   , additionalFonts = ["xft:DejaVu Sans Mono:size=10","xft:Symbola:size=10","xft:Code2000:size=10"]

   , bgColor     = "#408080"
   , fgColor     = "#FFFFFF"
   , borderColor = "#000000"

   , position = Top
   , border   = NoBorder

   -- layout
   , sepChar  = "%"
   , alignSep = "}{"
   , template = "%multicpu% | %memory% | %dynnetwork% } %StdinReader% { %dropbox% | %volOut% | %volIn% | %steam% | %date% | %time%"

   -- general behavior
   , allDesktops      = True    -- show on all desktops
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , pickBroadest     = False   -- choose widest display (multi-monitor)

   , commands =
        -- xmonad log message display
        [ Run $ XPropertyLog "_XMONAD_LOG"
        , Run $ StdinReader

        -- network activity monitor
        , Run $ DynNetwork [ "--template" , "<rx>▼ <tx>▲"
                           , "--Low"      , "1000"       -- units: kB/s
                           , "--High"     , "5000"       -- units: kB/s
                           , "--low"      , "green"
                           , "--normal"   , "orange"
                           , "--high"     , "red"
                           , "--minwidth" , "5"
                           ] 10

        -- cpu activity monitor
        , Run $ MultiCpu [ "--template" , "<autovbar>"
                         , "--Low"      , "50"         -- units: %
                         , "--High"     , "85"         -- units: %
                         , "--low"      , "green"
                         , "--normal"   , "orange"
                         , "--high"     , "red"
                         ] 10


        -- memory usage monitor
        , Run $ Memory [ "--template" ,"<usedratio>%"
                       , "--Low"      , "20"        -- units: %
                       , "--High"     , "90"        -- units: %
                       , "--low"      , "green"
                       , "--normal"   , "orange"
                       , "--high"     , "red"
                       , "--ppad"     , "2"
                       ] 10

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date "%d-%m-%Y (<fc=lightblue>%a</fc>)" "date" 10
        , Run $ Date "<fc=cyan>%T</fc>"                "time" 10

        -- volume info (requires my 'volstatus' command)
        , Run $ Com "volstatus" ["Capture", "IN"] "volIn" 10
        , Run $ Com "volstatus" ["Master",  "OUT"] "volOut" 10

        -- dropbox status display
        , Run $ Com "dropbox" ["status"] "dropbox" 10

        -- show who is online on steam (requires my 'steamutils' command)
        , Run $ Com "steamutils" ["--short"] "steam" 300
        ]
   }
