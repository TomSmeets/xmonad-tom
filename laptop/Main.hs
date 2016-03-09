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

conf = onStartup doBG
     . fixJava
     . withWSTree myTree
     . flip additionalKeysP [ ("M-u", void . runDialogX $ listToTree (Choice "" $ return ()) [ Choice "Wallpaper" doBG ]) ]
     $ myConfig

doBG :: X ()
doBG = spawn "FractalArt -f $HOME/.fractalart/wallpaper.bmp && feh --bg-fill $HOME/.fractalart/wallpaper.bmp"

myTree :: Tree Workspace
myTree = idx $ nd "root" [ nd "School" (numbers 0)
                         , nd "Browser" []
                         ]
  where
    nd x xs = Node (Workspace "" x True) xs
    numbers n = map (\c -> nd [c] []) . drop n $ ['a'..'f']
    idx = fmap (\(w, p) -> w{ path = intercalate "." p }) . treePath name
