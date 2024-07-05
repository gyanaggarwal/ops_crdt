module Control.CRDT.SDProduct where

import Control.Lens

import Data.CRDT
import Control.CRDT.SDProductUtil
import Control.CRDT.MsgData
import Control.CRDT.IntReg
import Control.CRDT.EDFlag
import Control.CRDT.ARSet
import Control.CRDT.MVReg

-- msg that are ignored because of concurrent Reset will still update the VectorClock
-- also add such a msg to the not csvc list of msgs
-- do we need to retain VectorClock msg NoOp and remove it when it becomes causally stable

updateResetCRDT :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                   CrdtType ->
                   MsgOps nid k v ->
                   MsgData nid k v ->
                   CrdtData nid k v ->
                   CrdtData nid k v
updateResetCRDT ctype msg mdata cdata = case allResetCRDT mdata' of
                                             True  -> makeDataWithMdata ctype mdata'
                                             False -> setMsgData ctype mdata' cdata
                                           where mdata' = appendAndFilterForResetCRDTMsg msg mdata

processMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                 CrdtType ->
                 SDPMsgOps nid k v ->
                 CrdtData nid k v ->
                 Maybe (CrdtData nid k v)
processMsgOps ctype (SDPReset, opx@MsgOps {_crdt_ops = ResetCRDT}) cdata = do
  mdata <- getMsgData ctype cdata
  return (updateResetCRDT ctype opx mdata cdata)
processMsgOps ctype (sdp, msg) cdata = do
  mdata <- getMsgData ctype cdata
  case anyResetCRDT msg mdata of
    False -> updateMsgOps ctype (sdp, msg) mdata cdata
    True  -> return cdata

setEFCheckData :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  VClock nid ->
                  FlagData ->
                  MsgData nid k v ->
                  CrdtData nid k v ->
                  CrdtData nid k v
setEFCheckData mvc fdata mdata cdata = setEFData fdata' mdata' cdata
                                         where mdata' = filterNotResetMsg filterCONLT mvc mdata
                                               fdata' = setFlagData mdata' fdata

setEFAddData :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                MsgOps nid k v ->
                FlagData ->
                MsgData nid k v ->
                CrdtData nid k v ->
                CrdtData nid k v
setEFAddData msg fdata mdata cdata = setEFData fdata' mdata' cdata
                                       where mdata' = appendMsg msg mdata
                                             fdata' = setAddFlag fdata

setEDCheckData :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  VClock nid ->
                  EDFlagData nid k v ->
                  EDFlagData nid k v
setEDCheckData mvc edata = setEDData fdata' mdata' edata
                             where mdata' = filterNotResetMsg filterCONLT mvc $ edata ^. edfMsgData
                                   fdata' = setFlagData mdata' $ edata ^. edFlagData

setEDAddData :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                MsgOps nid k v ->
                EDFlagData nid k v ->
                EDFlagData nid k v
setEDAddData msg edata = setEDData fdata' mdata' edata
                           where mdata' = appendMsg msg $ edata ^. edfMsgData
                                 fdata' = setAddFlag $ edata ^. edFlagData

updateEDMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  SDPMsgOps nid k v ->
                  Maybe (EDFlagData nid k v) ->
                  Maybe (EDFlagData nid k v)
updateEDMsgOps (SDPAddVC, msg) medata = do
  edata <- medata
  return (setEDAddData msg edata)
updateEDMsgOps (SDPCheckVC, MsgOps{_msg_vclock=mvc}) medata = do
  edata <- medata
  return (setEDCheckData mvc edata)
updateEDMsgOps _ _ = Nothing

updateASState :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                 CrdtType ->
                 (v, SDPMsgOps nid k v)->
                 ARSETData nid k v ->
                 ARSETData nid k v
updateASState ctype (val0, sdpmsg) asstate =
  asstate & at val0 ?~ (updateEDMsgOps sdpmsg medata)
    where medata = asstate ^. at val0 . non (makeEDFlagData ctype)

updateASMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  CrdtType ->
                  SDPMsgOps nid k v ->
                  MsgData nid k v ->
                  CrdtData nid k v ->
                  Maybe (CrdtData nid k v)
updateASMsgOps ctype (sdp, msg) mdata cdata = do
  bctype <- getBaseCRDT ctype
  (val0, bsdpmsg) <- getBaseMsgOps (sdp, msg)
  asState <- getASState cdata
  return (setASData (updateASState bctype (val0, bsdpmsg) asState) (appendMsg msg mdata) cdata)

updateEFMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  SDPMsgOps nid k v ->
                  MsgData nid k v ->
                  CrdtData nid k v ->
                  Maybe (CrdtData nid k v)
updateEFMsgOps (SDPAddVC, msg) mdata cdata = do
  fdata <- getEFState cdata
  return (setEFAddData msg fdata mdata cdata)
updateEFMsgOps (SDPCheckVC, MsgOps{_msg_vclock=mvc}) mdata cdata = do
  fdata <- getEFState cdata
  return (setEFCheckData mvc fdata mdata cdata)
updateEFMsgOps _ _ _ = Nothing

updateMRMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  SDPMsgOps nid k v ->
                  MsgData nid k v ->
                  CrdtData nid k v ->
                  Maybe (CrdtData nid k v)
updateMRMsgOps (SDPAddVC, opx@MsgOps{_msg_vclock = mvc}) mdata cdata = do
  let mdata' = filterAllMsg filterCONLT mvc mdata
  mvdata <- appendAssignVal (getAssignVal opx) (combineAssignVal mvc mdata')
  return (setMRData mvdata (appendMsg opx mdata') cdata)
updateMRMsgOps _ _ _ = Nothing

updateIRMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                  SDPMsgOps nid k v ->
                  MsgData nid k v ->
                  CrdtData nid k v ->
                  Maybe (CrdtData nid k v)
updateIRMsgOps (SDPAddVC, opx) mdata cdata =
  setIRData mirv mdata' cdata
    where mdata' = appendMsg opx mdata
          mirv = appMultIntProduct (getIRState cdata)  (getMultVal opx)
updateIRMsgOps (SDPCheckVC, opx@MsgOps{_msg_vclock = mvc}) mdata cdata =
  setIRData mirv mdata cdata
    where maiv = getAddVal opx
          mmfv = combineMultVal mvc mdata
          mirv = (+) <$> getIRState cdata <*> appMultIntProduct maiv mmfv
updateIRMsgOps _ _ _ = Nothing

updateMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                CrdtType ->
                SDPMsgOps nid k v ->
                MsgData nid k v ->
                CrdtData nid k v ->
                Maybe (CrdtData nid k v)
updateMsgOps IntReg msg mdata cdata = updateIRMsgOps msg mdata cdata

updateMsgOps EWFlag msg mdata cdata = updateEFMsgOps msg mdata cdata
updateMsgOps DWFlag msg mdata cdata = updateEFMsgOps msg mdata cdata

updateMsgOps AWSet msg mdata cdata  = updateASMsgOps AWSet msg mdata cdata
updateMsgOps RWSet msg mdata cdata  = updateASMsgOps RWSet msg mdata cdata

updateMsgOps MVReg msg mdata cdata = updateMRMsgOps msg mdata cdata
