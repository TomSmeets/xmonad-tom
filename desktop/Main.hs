module Main where

import Data.Tree
import XMonad
import XMonad.Tom
import XMonad.Tom.XMobarHs as XMobar
import XMonad.Util.EZConfig
import XMonad.Actions.TreeSelect

main :: IO ()
main = do
    export xmobarconf
    xmonad =<< withXMobar titlePP conf

conf = runInWS "firefox"                     "Left.Browser"
     . runInWS "vlc $HOME/Music/soundclound" "Right.Music"
     . onStartup doBG
     . dualScreen
     . fixJava
     . withWSTree myTree
     . flip additionalKeysP [ ("M-u", treeselectAction def
         [ Node (TSNode "Wallpaper" "Changer to a different wallpaper" doBG) []
         ])]
     $ myConfig

doBG :: X ()
doBG = spawn "FractalArt -w 1920 -h 1200 -f $HOME/.fractalart/wallpaperL.bmp \
           \& FractalArt -w 1920 -h 1080 -f $HOME/.fractalart/wallpaperR.bmp & wait \
           \&& feh --bg-fill $HOME/.fractalart/wallpaperL.bmp --bg-fill $HOME/.fractalart/wallpaperR.bmp"

myTree :: Forest String
myTree = [ Node "Left"
             [ Node "Browser"     []
             , Node "Programming" numbers
             , Node "Home"        numbers
             , Node "Game"        numbers
             ]
         , Node "Right"
             [ Node "Docs"     numbers
             , Node "Music"    []
             , Node "Video"    []
             , Node "Steam"    []
             , Node "Skype"    []
             , Node "Telegram" []
             ]
         ]
  where
    numbers = map (\c -> Node [c] []) ['a'..'f']

xmobarconf :: XMobar.Config
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
    , template = "%multicpu% %memory% | %dynnetwork% | %StdinReader% }{ %dropbox% | %steam% | %volOut%/%volIn% | %date% | %time%"

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
         , Run $ Com "steamutils" ["--short"] "steam" 600
         ]
    }
