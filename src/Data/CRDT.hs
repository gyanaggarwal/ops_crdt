{-# LANGUAGE TemplateHaskell #-}

module Data.CRDT where

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup

type TCLUSTER_VER   = Int
type TLOGICAL_CLOCK = Int
type TIntVal        = Int
type TMVReg v       = [v]

newtype LOGICAL_CLOCK = LOGICAL_CLOCK TLOGICAL_CLOCK
                        deriving (Show, Ord, Eq)

instance Semigroup LOGICAL_CLOCK where
  (LOGICAL_CLOCK lc0) <> (LOGICAL_CLOCK lc1) = LOGICAL_CLOCK (lc0+lc1)

instance Monoid LOGICAL_CLOCK where
  mempty = LOGICAL_CLOCK 0

type VClock nid = Map.Map nid LOGICAL_CLOCK

type PVClock nid = Map.Map nid (VClock nid)

data CVClock = LTVC | EQVC | GTVC | CONVC
               deriving (Show, Eq)

emptySet :: Set.Set k
emptySet = Set.empty

emptyVC :: VClock nid
emptyVC = Map.empty

emptyPVC :: PVClock nid
emptyPVC = Map.empty

data EDFlag = Disabled | Enabled
              deriving (Show, Eq)

data SDProduct = SDPReset | SDPCheckVC| SDPAddVC
                 deriving (Show, Ord, Eq)

data CrdtType =   IntReg
                | EWFlag
                | DWFlag
                | AWSet
                | RWSet
                | MVReg
--                | AWMap
--                | RWMap
--                | JSON
                deriving (Show)

data AddVal =   AddVal  {_addval :: TIntVal}
                deriving (Show, Eq)
makeLenses ''AddVal

data MultVal = MultVal {_multval :: Product TIntVal}
               deriving (Show, Eq)
makeLenses ''MultVal

data AddElem v = AddElem {_addelem :: v}
                 deriving (Show, Eq)
makeLenses ''AddElem

data RmvElem v = RmvElem {_rmvelem :: v}
                 deriving (Show, Eq)
makeLenses ''RmvElem

data AssignVal v = AssignVal {_assignval :: v}
                   deriving (Show, Eq)
makeLenses ''AssignVal

data CrdtOps k v =   ADDVal AddVal
                   | MULTVal MultVal
                   | Enable
                   | Disable
                   | ADDElem (AddElem v)
                   | RMVElem (RmvElem v)
                   | ASSIGNVal (AssignVal v)
                   | ResetCRDT
--                   | Update  {key :: k, value :: v}
--                   | Remove  {key :: k}
--                   | NoOp
                   deriving (Show, Eq)
makePrisms ''CrdtOps

data MsgOps nid k v =  MsgOps {_msg_node_id :: nid,
                               _msg_vclock :: VClock nid,
                               _crdt_ops :: CrdtOps k v}
                       deriving (Show, Eq)
makeLenses ''MsgOps

type SDPMsgOps nid k v = (SDProduct, MsgOps nid k v)

data FlagData = FlagData {_flagValue :: EDFlag,
                          _checkflagValue :: EDFlag,
                          _addflagValue :: EDFlag}
                deriving (Show, Eq)
makeLenses ''FlagData

type MsgData nid k v = [MsgOps nid k v]

data IntRegData nid k v = IntRegData {_intRegData :: TIntVal,
                                      _irgMsgData :: MsgData nid k v}
                          deriving (Show, Eq)
makeLenses ''IntRegData

data MVRegData nid k v = MVRegData {_mvRegData :: [v],
                                    _mvrMsgData :: MsgData nid k v}
                         deriving (Show, Eq)
makeLenses ''MVRegData

data EDFlagData nid k v = EDFlagData {_edFlagData :: FlagData,
                                      _edfMsgData :: MsgData nid k v}
                          deriving (Show, Eq)
makeLenses ''EDFlagData

type ARSETData nid k v = Map.Map v (Maybe (EDFlagData nid k v))

emptyARSETData :: ARSETData nid k v
emptyARSETData = Map.empty

data ARSetData nid k v = ARSetData {_arSetData  :: ARSETData nid k v,
                                    _arsMsgData :: MsgData nid k v}
                         deriving (Show, Eq)
makeLenses '' ARSetData

data CrdtData nid k v =   IRData (IntRegData nid k v)
                        | EFData (EDFlagData nid k v)
                        | ASData (ARSetData nid k v)
                        | MRData (MVRegData nid k v)
                        deriving (Show)
makePrisms ''CrdtData
