module Control.CRDT.MsgData where

import Control.Lens

import Data.CRDT
import Control.CRDT.VectorClock

emptyMsgData :: MsgData nid k v
emptyMsgData = []

filterCON :: [Maybe CVClock]
filterCON = [Just CONVC]

filterCONLT :: [Maybe CVClock]
filterCONLT = [Just LTVC, Just CONVC]

makeMsgOps :: nid -> VClock nid -> CrdtOps k v -> MsgOps nid k v
makeMsgOps mnd mvc cops = MsgOps {_msg_node_id = mnd, _msg_vclock = mvc, _crdt_ops = cops}

appendMsg :: MsgOps nid k v -> MsgData nid k v -> MsgData nid k v
appendMsg msg mdata = msg : mdata

isResetCRDT :: MsgOps nid k v -> Bool
isResetCRDT msg = msg ^. crdt_ops ^? _ResetCRDT /= Nothing

isNotResetCRDT :: MsgOps nid k v -> Bool
isNotResetCRDT msg = not $ isResetCRDT msg

keepResetCRDT :: Ord nid => Bool -> VClock nid -> MsgOps nid k v -> Bool
keepResetCRDT bv mvc msg =
  case (cmpVC mvc $ msg ^. msg_vclock) of
    Just LTVC  -> bv
    Just CONVC -> isResetCRDT msg
    Just EQVC  -> isResetCRDT msg
    Just GTVC  -> False
    Nothing    -> False

checkVCMsg :: (Ord nid, Foldable f) =>
              (MsgOps nid k v -> Bool) ->
              (f (Maybe CVClock) -> VClock nid -> VClock nid -> Bool) ->
              f (Maybe CVClock) -> VClock nid -> MsgOps nid k v -> Bool
checkVCMsg mfun cfun fmc mvc msg = mfun msg && (cfun fmc mvc $ msg ^. msg_vclock)

filterMsg :: Ord nid => (MsgOps nid k v -> Bool) -> MsgData nid k v -> MsgData nid k v
filterMsg fmfun mdata = mdata ^.. folded . filtered fmfun

checkAndFilterMsg :: (Ord nid, Foldable f) =>
                     (MsgOps nid k v -> Bool) ->
                     f (Maybe CVClock) ->
                     VClock nid ->
                     MsgData nid k v ->
                     MsgData nid k v
checkAndFilterMsg cfun fmc mvc mdata = filterMsg (checkVCMsg cfun cmpVCFor fmc mvc) mdata

filterAllMsg :: (Ord nid, Foldable f) =>
                f (Maybe CVClock) ->
                VClock nid ->
                MsgData nid k v ->
                MsgData nid k v
filterAllMsg fmc mvc mdata = checkAndFilterMsg (const True) fmc mvc mdata

filterNotResetMsg :: (Ord nid, Foldable f) =>
                     f (Maybe CVClock) ->
                     VClock nid ->
                     MsgData nid k v ->
                     MsgData nid k v
filterNotResetMsg fmc mvc mdata = checkAndFilterMsg isNotResetCRDT fmc mvc mdata

filterNotResetCONMsgData :: (Ord nid) =>
                            VClock nid ->
                            MsgData nid k v ->
                            MsgData nid k v
filterNotResetCONMsgData mvc mdata = filterNotResetMsg filterCON mvc mdata

appendAndFilterForResetCRDTMsg :: Ord nid =>
                                  MsgOps nid k v ->
                                  MsgData nid k v ->
                                  MsgData nid k v
appendAndFilterForResetCRDTMsg msg mdata = msg : filterMsg (keepResetCRDT True $ msg ^. msg_vclock) mdata

allResetCRDT :: MsgData nid k v -> Bool
allResetCRDT mdata = allOf folded isResetCRDT mdata

anyResetCRDT :: Ord nid => MsgOps nid k v -> MsgData nid k v -> Bool
anyResetCRDT msg mdata = anyOf folded (keepResetCRDT False $ msg ^. msg_vclock) mdata
