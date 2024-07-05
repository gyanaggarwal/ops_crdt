module Control.CRDT.SDProductUtil where

import Data.CRDT
import Control.CRDT.MsgData
import Control.CRDT.IntReg
import Control.CRDT.EDFlag
import Control.CRDT.ARSet
import Control.CRDT.MVReg

validateMsg :: CrdtType -> MsgOps nid k v -> Maybe (SDPMsgOps nid k v)
validateMsg _      opx@MsgOps{_crdt_ops = ResetCRDT}   = pure (SDPReset,   opx)
validateMsg IntReg opx@MsgOps{_crdt_ops = ADDVal{}}    = pure (SDPCheckVC, opx)
validateMsg IntReg opx@MsgOps{_crdt_ops = MULTVal{}}   = pure (SDPAddVC,   opx)
validateMsg EWFlag opx@MsgOps{_crdt_ops = Disable}     = pure (SDPCheckVC, opx)
validateMsg EWFlag opx@MsgOps{_crdt_ops = Enable}      = pure (SDPAddVC,   opx)
validateMsg DWFlag opx@MsgOps{_crdt_ops = Enable}      = pure (SDPCheckVC, opx)
validateMsg DWFlag opx@MsgOps{_crdt_ops = Disable}     = pure (SDPAddVC,   opx)
validateMsg AWSet  opx@MsgOps{_crdt_ops = RMVElem{}}   = pure (SDPCheckVC, opx)
validateMsg AWSet  opx@MsgOps{_crdt_ops = ADDElem{}}   = pure (SDPAddVC,   opx)
validateMsg RWSet  opx@MsgOps{_crdt_ops = ADDElem{}}   = pure (SDPCheckVC, opx)
validateMsg RWSet  opx@MsgOps{_crdt_ops = RMVElem{}}   = pure (SDPAddVC,   opx)
validateMsg MVReg  opx@MsgOps{_crdt_ops = ASSIGNVal{}} = pure (SDPAddVC,   opx)
validateMsg _      _                                   = Nothing

makeEDFlagData :: CrdtType -> Maybe (EDFlagData nid k v)
makeEDFlagData EWFlag = pure (EDFlagData {_edFlagData = makeEWFlagData, _edfMsgData = emptyMsgData})
makeEDFlagData DWFlag = pure (EDFlagData {_edFlagData = makeDWFlagData, _edfMsgData = emptyMsgData})
makeEDFlagData _      = Nothing

makeDataWithMdata :: CrdtType -> MsgData nid k v -> CrdtData nid k v
makeDataWithMdata IntReg mdata = IRData (IntRegData {_intRegData = 1 :: TIntVal,   _irgMsgData = mdata})
makeDataWithMdata EWFlag mdata = EFData (EDFlagData {_edFlagData = makeEWFlagData, _edfMsgData = mdata})
makeDataWithMdata DWFlag mdata = EFData (EDFlagData {_edFlagData = makeDWFlagData, _edfMsgData = mdata})
makeDataWithMdata AWSet  mdata = ASData (ARSetData  {_arSetData  = makeSetData,    _arsMsgData = mdata})
makeDataWithMdata RWSet  mdata = ASData (ARSetData  {_arSetData  = makeSetData,    _arsMsgData = mdata})
makeDataWithMdata MVReg  mdata = MRData (MVRegData  {_mvRegData  = [] :: TMVReg v, _mvrMsgData = mdata})

makeData :: CrdtType -> CrdtData nid k v
makeData ctype = makeDataWithMdata ctype emptyMsgData

makeBaseCRDT :: CrdtType -> Maybe (CrdtData nid k v)
makeBaseCRDT ctype = makeData <$> getBaseCRDT ctype

getMsgData :: CrdtType -> CrdtData nid k v -> Maybe (MsgData nid k v)
getMsgData IntReg cdata = getIRMsgData cdata
getMsgData EWFlag cdata = getEFMsgData cdata
getMsgData DWFlag cdata = getEFMsgData cdata
getMsgData AWSet  cdata = getASMsgData cdata
getMsgData RWSet  cdata = getASMsgData cdata
getMsgData MVReg  cdata = getMRMsgData cdata

setMsgData :: CrdtType -> MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setMsgData IntReg mdata cdata = setIRMsgData mdata cdata
setMsgData EWFlag mdata cdata = setEFMsgData mdata cdata
setMsgData DWFlag mdata cdata = setEFMsgData mdata cdata
setMsgData AWSet  mdata cdata = setASMsgData mdata cdata
setMsgData RWSet  mdata cdata = setASMsgData mdata cdata
setMsgData MVReg  mdata cdata = setMRMsgData mdata cdata
