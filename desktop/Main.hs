module Main where

import Control.Monad
import Data.List
import Data.Tree
import Data.Tree.Zipper
import XMonad
import XMonad.Tom
import XMonad.Tom.UI.Dialog
import XMonad.Tom.Workspace
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< withXMobar titlePP conf

conf = runInWS "iceweasel" "root.Left.Browser"
     . onStartup doBG
     . dualScreen 
     . fixJava 
     . withWSTree myTree
     . flip additionalKeysP [ ("M-u", void . runDialogX $ listToTree (Choice "" $ return ()) [ Choice "Wallpaper" doBG ]) ]
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
