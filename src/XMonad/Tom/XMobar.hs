module XMonad.Tom.XMobar where

import XMonad.Tom.XMobarHs

bg = "#408080"
fg = "#FFFFFF"

mkTemplate = ("%memory%", fg, "red") $> ("%multicpu%", fg, "blue") ++ "} Test { Test"
  where
    ($>) (l, fa, ba) (r, fb, bb) = col fa ba l ++ col ba bb "▶" ++ col fb bb r
    col fg bg s = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ s ++ "</fc>"

config = defaultConfig 
   { font            = "xft:monospace:size=10:bold"
   , additionalFonts = ["xft:DejaVu Sans Mono:size=10","xft:Symbola:size=10","xft:Code2000:size=10"]

   , bgColor     = bg
   , fgColor     = fg
   , borderColor = "#000000"

   , position    = Top
   , border      = NoBorder

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "%dynnetwork% | %memory% %multicpu% } %StdinReader% { %dropbox% | %volOut% | %volIn% | %steam% | %date%"

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 
        -- network activity monitor (dynamic interface resolution)
        [ Run $ DynNetwork     [ "--template" , "<rx>▼ <tx>▲"
                             , "--Low"      , "1000"       -- units: kB/s
                             , "--High"     , "5000"       -- units: kB/s
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             , "--minwidth" , "5"
                             ] 10
        , Run $ StdinReader

        -- cpu activity monitor
        , Run $ MultiCpu       [ "--template" , "<autovbar>"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             , "--ppad"     , "3"
                             ] 10
                            
        , Run $ XPropertyLog "_XMONAD_LOG"
        -- cpu core temperature monitor
        , Run $ CoreTemp       [ "--template" , "<autovbar>"
                             , "--Low"      , "40"        -- units: °C
                             , "--High"     , "60"        -- units: °C
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             , "--minwidth" , "3"
                             ] 10
                          
        -- memory usage monitor
        , Run $ Memory         [ "--template" ,"<usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             , "--ppad"     , "3"
                             ] 10

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date "%F %T" "date" 10
        , Run $ Com "volstatus" ["Capture", "IN"] "volIn" 10
        , Run $ Com "volstatus" ["Master",  "OUT"] "volOut" 10
        , Run $ Com "dropbox" ["status"] "dropbox" 10
        , Run $ Com "steamutils" ["--short"] "steam" 300
        ]
   }
