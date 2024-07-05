module Control.CRDT.MVReg where

import Control.Lens

import Data.CRDT
import Control.CRDT.MsgData

assignVal :: v -> CrdtOps k v
assignVal a = ASSIGNVal $ AssignVal {_assignval = a}

getMRState :: CrdtData nid k v -> Maybe (TMVReg v)
getMRState cdata = do
                     mrData <- cdata ^? _MRData
                     return (mrData ^. mvRegData)

getMRMsgData :: CrdtData nid k v -> Maybe (MsgData nid k v)
getMRMsgData cdata = do
                       mrdata <- cdata ^? _MRData
                       return (mrdata ^. mvrMsgData)

setMRMsgData :: MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setMRMsgData mdata cdata = cdata & _MRData . mvrMsgData .~ mdata

setMRData :: [v] -> MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setMRData vs mdata cdata = cdata & _MRData . mvrMsgData .~ mdata
                                 & _MRData . mvRegData .~ vs

getAssignVal :: MsgOps nid k v -> Maybe v
getAssignVal msg = do
                     asgn01 <- msg ^. crdt_ops ^? _ASSIGNVal
                     return (asgn01 ^. assignval)

memptyList :: Maybe [v]
memptyList = pure []

appendAssignVal :: Maybe v -> Maybe (TMVReg v) -> Maybe (TMVReg v)
appendAssignVal mval mvals = (:) <$> mval <*> mvals

combineAssignVal :: Ord nid => VClock nid -> MsgData nid k v -> Maybe (TMVReg v)
combineAssignVal mvc mdata = foldrOf folded appendAssignVal memptyList
                               $ (filterNotResetCONMsgData mvc mdata) ^.. folded . to getAssignVal

queryMVReg :: Maybe (CrdtData nid k v) -> Maybe (TMVReg v)
queryMVReg mcdata = do
                      cdata <- mcdata
                      getMRState cdata
