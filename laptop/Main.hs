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
main = do
    export xmobarconf
    xmonad =<< withXMobar titlePP conf

conf = onStartup doBG
     . fixJava
     . withWSTree myTree
     . flip additionalKeysP [ ("M-u", void . runDialogX $ listToTree (Choice "" $ return ())
           [ Choice "Bright" (spawn "xbacklight -set 99 -time 1; xbacklight -set 100")
           , Choice "Dim"    (spawn "xbacklight -set 24 -time 1; xbacklight -set 25")
           , Choice "Wallpaper" doBG
           ]) ]
     $ myConfig

doBG :: X ()
doBG = spawn "FractalArt -f $HOME/.fractalart/wallpaper.bmp && feh --bg-fill $HOME/.fractalart/wallpaper.bmp"

myTree :: Tree Workspace
myTree = idx $ nd "root" [ nd "School" (numbers 0)
                         , nd "Browser" []
                         , nd "Home"   (numbers 0)
                         ]
  where
    nd x xs = Node (Workspace "" x True) xs
    numbers n = map (\c -> nd [c] []) . drop n $ ['a'..'f']
    idx = fmap (\(w, p) -> w{ path = intercalate "." p }) . treePath name

xmobarconf :: Config
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
    , template = "%multicpu% | %memory% | %dynnetwork% | %StdinReader% }{ %dropbox% | %battery% | %date% | %secs% | %time%"

    -- general behavior
    , allDesktops      = True    -- show on all desktops
    , hideOnStart      = False   -- start with window unmapped (hidden)
    , lowerOnStart     = True    -- send to bottom of window stack on start
    , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
    , persistent       = True    -- enable/disable hiding (True = disabled)
    , pickBroadest     = False   -- choose widest display (multi-monitor)

    , commands = [ Run $ XPropertyLog "_XMONAD_LOG"
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
                 , Run $ Date "<fc=cyan>%T</fc>"                 "time" 10
                 , Run $ Com "/bin/bash" ["-c", "( date --date 'june 19' +'%s' &&  date +'%s' && echo '-p') | dc"] "secs" 10

                 -- dropbox status display
                 , Run $ Com "dropbox" ["status"] "dropbox" 10

                 -- battery monitor in percent
                 , Run $ Com "cat" ["/sys/class/power_supply/BAT1/capacity"] "battery" 10
                 ]
   }
