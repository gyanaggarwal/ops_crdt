module Control.CRDT.ManageCSVC where

import Control.Lens

import Data.CRDT
import Control.CRDT.TRCB
import Control.CRDT.SDProductUtil
import Control.CRDT.MsgData
import Control.CRDT.ARSet

causallyStableVC :: (Ord nid, Eq nid) => NodeTRCB nid -> VClock nid
causallyStableVC trcb = mergeTRCB min trcb

removeCSVCAndInsertARSet :: (Ord nid, Ord v) =>
                            VClock nid ->
                            v ->
                            EDFlagData nid k v ->
                            ARSETData nid k v ->
                            ARSETData nid k v
removeCSVCAndInsertARSet csvc val edata arsdata =
  arsdata & ix val .~ (pure edata')
    where mdata = filterAllMsg filterCONLT csvc $ edata ^. edfMsgData
          edata' = edata & edfMsgData .~ mdata

removeCSVCAndMakeARSet :: (Ord nid, Ord v) =>
                          VClock nid ->
                          ARSETData nid k v ->
                          Maybe (ARSETData nid k v)
removeCSVCAndMakeARSet csvc arsData =
  ifoldrOf ifolded (\vv medata arsdata0 ->
    (removeCSVCAndInsertARSet csvc vv) <$> medata <*> arsdata0) (pure emptyARSETData) arsData

removeCSVCARSet :: (Ord nid, Ord v) =>
                   CrdtType ->
                   VClock nid ->
                   CrdtData nid k v ->
                   Maybe (CrdtData nid k v)
removeCSVCARSet ctype csvc cdata = do
                                     mdata <- getMsgData ctype cdata
                                     arsData <- getASState cdata
                                     arsData' <- removeCSVCAndMakeARSet csvc arsData
                                     return $ setASData arsData' (filterAllMsg filterCONLT csvc mdata) cdata

removeCSVC :: (Ord nid, Ord k, Ord v) =>
              CrdtType ->
              VClock nid ->
              CrdtData nid k v ->
              Maybe (CrdtData nid k v)
removeCSVC AWSet csvc cdata = removeCSVCARSet AWSet csvc cdata
removeCSVC RWSet csvc cdata = removeCSVCARSet RWSet csvc cdata
removeCSVC ctype csvc cdata = do
                                mdata <- getMsgData ctype cdata
                                return $ setMsgData ctype (filterAllMsg filterCONLT csvc mdata) cdata
