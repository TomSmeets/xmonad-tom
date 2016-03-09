module XMonad.Tom.Workspace.History where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

newtype WSHistory = WSHistory { getHistory :: ([String], String, [String]) } deriving (Read,Show,Typeable)

instance ExtensionClass WSHistory where
    initialValue  = WSHistory ([], "", [])
    extensionType = PersistentExtension

goToWS, moveToWS :: String -> X ()
goToWS   i = (windows $ W.greedyView (i))                    >> addUndo i
moveToWS i = (windows $ W.shift (i))

addUndo :: String -> X ()
addUndo i = XS.get >>= addUndo' i . getHistory

doUndo, doRedo :: X ()
doUndo = undo >>= mapM_ (windows . W.greedyView)
doRedo = redo >>= mapM_ (windows . W.greedyView)

undo, redo :: X (Maybe String)
undo = XS.get >>= undo' . getHistory
redo = XS.get >>= redo' . getHistory

addUndo' :: String -> ([String], String, [String]) -> X ()
addUndo' i (us, c, rs) | c == i    = return ()
                       | otherwise = XS.put (WSHistory (c:us, i, [])) -- maybe do not remove redos

undo', redo' :: ([String], String, [String]) -> X (Maybe String)
undo' (u:us, c, rs) = XS.put (WSHistory (us, u, c:rs)) >> return (Just u)
undo' ([],   c, rs) = XS.put (WSHistory ([], c, rs))   >> return Nothing

redo' (us, c, r:rs) = XS.put (WSHistory (c:us, r, rs)) >> return (Just r)
redo' (us, c, [])   = XS.put (WSHistory (us, c, []))   >> return Nothing
