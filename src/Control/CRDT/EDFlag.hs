module Control.CRDT.EDFlag where

import Control.Lens

import Data.CRDT

makeEWFlagData :: FlagData
makeEWFlagData = FlagData {_flagValue = Disabled, _checkflagValue = Disabled, _addflagValue = Enabled}

makeDWFlagData :: FlagData
makeDWFlagData = FlagData {_flagValue = Enabled,  _checkflagValue = Enabled,  _addflagValue = Disabled}

isEnabled :: FlagData -> Bool
isEnabled fdata = fdata ^. flagValue == Enabled

setAddFlag :: FlagData -> FlagData
setAddFlag fdata = fdata & flagValue .~ (fdata ^. addflagValue)

setCheckFlag :: FlagData -> FlagData
setCheckFlag fdata = fdata & flagValue .~ (fdata ^. checkflagValue)

setFlagData :: MsgData nid k v -> FlagData -> FlagData
setFlagData mdata fdata = case length mdata of
                            0 -> setCheckFlag fdata
                            _ -> fdata

getEFState :: CrdtData nid k v -> Maybe FlagData
getEFState cdata = do
                     efData <- cdata ^? _EFData
                     return (efData ^. edFlagData)

getEFMsgData :: CrdtData nid k v -> Maybe (MsgData nid k v)
getEFMsgData cdata = do
                       efData <- cdata ^? _EFData
                       return (efData ^. edfMsgData)

setEFMsgData :: MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setEFMsgData mdata cdata = cdata & _EFData . edfMsgData .~ mdata

setEFData :: FlagData -> MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setEFData fdata mdata cdata = cdata & _EFData . edfMsgData .~ mdata
                                    & _EFData . edFlagData .~ fdata

setEDData :: FlagData -> MsgData nid k v -> EDFlagData nid k v -> EDFlagData nid k v
setEDData fdata mdata edata = edata & edfMsgData .~ mdata
                                    & edFlagData .~ fdata

queryEDFlag :: Maybe (CrdtData nid k v) -> Maybe EDFlag
queryEDFlag mcdata = do
                       cdata <- mcdata
                       fdata <- getEFState cdata
                       return (fdata ^. flagValue)
