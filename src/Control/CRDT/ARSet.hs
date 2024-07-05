module Control.CRDT.ARSet where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative

import Data.CRDT
import Control.CRDT.MsgData
import Control.CRDT.EDFlag

addElem :: v -> CrdtOps k v
addElem a = ADDElem $ AddElem {_addelem = a}

rmvElem :: v -> CrdtOps k v
rmvElem r = RMVElem $ RmvElem {_rmvelem = r}

makeSetData :: ARSETData nid k v
makeSetData = Map.empty

getASState :: CrdtData nid k v -> Maybe (ARSETData nid k v)
getASState cdata = do
                     asData <- cdata ^? _ASData
                     return (asData ^. arSetData)

getASMsgData :: CrdtData nid k v -> Maybe (MsgData nid k v)
getASMsgData cdata = do
                       asData <- cdata ^? _ASData
                       return (asData ^. arsMsgData)

setASMsgData :: MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setASMsgData mdata cdata = cdata & _ASData . arsMsgData .~ mdata

setARSetData :: ARSETData nid k v -> CrdtData nid k v -> CrdtData nid k v
setARSetData ardata cdata = cdata & _ASData . arSetData .~ ardata

setASData :: ARSETData nid k v -> MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setASData ardata mdata cdata = cdata & _ASData . arsMsgData .~ mdata
                                     & _ASData . arSetData .~ ardata

getBaseCRDT :: CrdtType -> Maybe CrdtType
getBaseCRDT AWSet = pure EWFlag
getBaseCRDT RWSet = pure DWFlag
getBaseCRDT _     = Nothing

getBaseCrdtOps :: CrdtOps k v -> Maybe (CrdtOps k v)
getBaseCrdtOps ADDElem{} = pure Enable
getBaseCrdtOps RMVElem{} = pure Disable
getBaseCrdtOps _         = Nothing

getAddElemVal :: CrdtOps k v -> Maybe v
getAddElemVal msg = (^. addelem) <$> (msg ^? _ADDElem)

getRmvElemVal :: CrdtOps k v -> Maybe v
getRmvElemVal msg = (^. rmvelem) <$> (msg ^? _RMVElem)

getARValue :: CrdtOps k v -> Maybe v
getARValue msg = getAddElemVal msg <|> getRmvElemVal msg

getBaseMsgOps :: SDPMsgOps nid k v -> Maybe (v, SDPMsgOps nid k v)
getBaseMsgOps (sdp, msg) = (,) <$> mvalue <*> sdpmsg
                             where cops = msg ^. crdt_ops
                                   mvalue = getARValue cops
                                   mmsg = (makeMsgOps (msg ^. msg_node_id) (msg ^. msg_vclock)) <$>
                                          getBaseCrdtOps cops
                                   sdpmsg = (,) <$> pure sdp <*> mmsg

insertIntoSet :: Ord v => v -> EDFlagData nid k v -> Set.Set v -> Set.Set v
insertIntoSet vv edata kvset = case isEnabled $ edata ^. edFlagData of
                                 True  -> Set.insert vv kvset
                                 False -> kvset

makeQuerySet :: Ord v => ARSETData nid k v -> Maybe (Set.Set v)
makeQuerySet arsData = ifoldrOf ifolded (\vv medata vvset0 ->
                         (insertIntoSet vv) <$> medata <*> vvset0) (pure emptySet) arsData

queryARSet :: Ord v => Maybe (CrdtData nid k v) -> Maybe (Set.Set v)
queryARSet mcdata = do
                      cdata <- mcdata
                      astate <- getASState cdata
                      makeQuerySet astate
