{-# LANGUAGE TemplateHaskell #-}
module XMonad.Tom.Workspace where

import Control.Monad.State
import Control.Applicative
import Data.Tree
import Data.Maybe
import Data.Tree.Zipper

data Workspace = Workspace { index :: Int, name :: String, folded :: Bool } deriving Show

type ZTree     = TreePos Full

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
    nd x xs = Node (Workspace 0 x True) xs
    numbers n = map (\c -> nd [c] []) . drop n $ ['a'..'f']
    idx = fmap (\(w, i) -> w{ index = i }) . flatIdx


unfoldParents :: ZTree Workspace -> ZTree Workspace
unfoldParents ws = doUp (setFold False) ws

doUp :: (a -> a) -> ZTree a -> ZTree a
doUp f x = fromMaybe x go
  where
    go = (prev x   >>= (return . doUp f)                 >>= next)
     <|> (parent x >>= (return . doUp f . modifyLabel f) >>= child)

doTop :: (ZTree a -> ZTree a) -> ZTree a -> ZTree a
doTop f x = fromMaybe x go
  where
    go = (prev x   >>= (return . doTop f)                 >>= next)
     <|> (parent x >>= (return . doTop f) >>= child)
     <|> Just (f x)

doAll f = doTop (fromTree . fmap f . toTree)

flatIdx :: Tree a -> Tree (a, Int)
flatIdx tr = evalState (idx tr) 0
  where
    idx :: Tree a -> State Int (Tree (a, Int))
    idx (Node x xs) = do
        n <- get
        modify (+1)
        ch <- mapM idx xs
        return $ Node (x,n) ch

-- | Convert a Tree to a list with an indent level
toIndents :: Tree Workspace -> [(Workspace, Integer)]
toIndents (Node w ws) | folded w  = [(w, 0)]
                      | otherwise = (w, 0) : concatMap (map inc . toIndents) ws
  where
    inc (x, i) = (x, i+1)

-- ** Actions

-- | Toggle folded, see 'setFold'
toggleFold :: Workspace -> Workspace
toggleFold w = setFold (not $ folded w) w

-- | Set the folding on or off
setFold :: Bool -> Workspace -> Workspace
setFold b w = w{ folded = b }

-- | Go to the first node that matches f
searchBelow :: (a -> Bool) -> ZTree a -> Maybe (ZTree a)
searchBelow f = go
  where
    go t | f $ label t       = Just t
         | otherwise = (child t >>= go)
                   <|> (next  t >>= go)

-- | Go to first child
child :: ZTree a -> Maybe (ZTree a)
child = nextTree . children

-- | a flattend list of nodes above this one
above :: ZTree a -> [a]
above z = concatMap extract (parents z) ++ (concatMap flatten $ before z)
  where
    extract (a, b, _) = concatMap flatten a ++ [b]

-- | a flattend list of nodes below this one
below :: ZTree a -> [a]
below z = concatMap extract (parents z) ++ (concatMap flatten $ after z)
  where
    extract (_, _, c) = concatMap flatten c

-- | flatten this zipper, see 'Data.Tree.flatten'
flattenZ :: ZTree a -> [a]
flattenZ = flatten . toTree
