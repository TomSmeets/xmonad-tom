module XMonad.Tom.Workspace.History where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

newtype WSHistory = WSHistory { getHistory :: ([Int], Int, [Int]) } deriving (Read,Show,Typeable)

instance ExtensionClass WSHistory where
    initialValue  = WSHistory ([], 0, [])
    extensionType = PersistentExtension

goToWS, moveToWS :: Int -> X ()
goToWS   i = (windows $ W.greedyView (show i))                    >> addUndo i
moveToWS i = (windows $ W.shift (show i))

addUndo :: Int -> X ()
addUndo i = XS.get >>= addUndo' i . getHistory

doUndo, doRedo :: X ()
doUndo = undo >>= mapM_ (windows . W.greedyView . show)
doRedo = redo >>= mapM_ (windows . W.greedyView . show)

undo, redo :: X (Maybe Int)
undo = XS.get >>= undo' . getHistory
redo = XS.get >>= redo' . getHistory

addUndo' :: Int -> ([Int], Int, [Int]) -> X ()
addUndo' i (us, c, rs) | c == i    = return ()
                       | otherwise = XS.put (WSHistory (c:us, i, [])) -- maybe do not remove redos

undo', redo' :: ([Int], Int, [Int]) -> X (Maybe Int)
undo' (u:us, c, rs) = XS.put (WSHistory (us, u, c:rs)) >> return (Just u)
undo' ([],   c, rs) = XS.put (WSHistory ([], c, rs))   >> return Nothing

redo' (us, c, r:rs) = XS.put (WSHistory (c:us, r, rs)) >> return (Just r)
redo' (us, c, [])   = XS.put (WSHistory (us, c, []))   >> return Nothing
