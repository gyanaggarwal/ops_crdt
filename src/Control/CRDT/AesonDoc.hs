{-# LANGUAGE OverloadedStrings #-}

module Control.CRDT.AesonDoc where

import GHC.Exts

import Data.Aeson
import Data.Aeson.Lens
--import Data.Functor.Identity
import qualified Data.Text as DT

emptyAesonMap :: Value
emptyAesonMap = Object $ fromList []

nullAeson :: Value
nullAeson = Null

sampleVal1 :: Value
sampleVal1 = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3, Bool False]),
  ("boolean", Bool True) ]


emptyAesonList :: Value
emptyAesonList = Array $ fromList []

data AesonACC =   AKEY
                | ANTH
                deriving (Ord, Eq, Show)

data CMPPV =   EQPV
             | NEQPV
             | HNPV
             | LNPV
             deriving (Ord, Eq, Show)

data PathValue = PathValue {aesonAcc :: AesonACC,
                            textAcc  :: DT.Text,
                            intAcc   :: Int}
--                                accPath  :: (Value -> f Value) -> t -> f t}
                 deriving (Ord, Eq, Show)

type AccPath = [PathValue]

cmpPV :: PathValue -> PathValue -> CMPPV
cmpPV pv0 pv1 =
  case pv0 == pv1 of
    True  -> EQPV
    False -> NEQPV

cmpPath :: AccPath -> AccPath -> CMPPV -> CMPPV
cmpPath []         []         cmppv = cmppv
cmpPath []          _         EQPV  = HNPV
cmpPath _          []         EQPV  = LNPV
cmpPath (pv0:pvs0) (pv1:pvs1) EQPV  = cmpPath pvs0 pvs1 (cmpPV pv0 pv1)
cmpPath _           _         cmppv = cmppv

--makePath PathValue{aesonAcc = AKEY, textAcc = tacc} = key tacc
--makePath PathValue{aesonAcc = ANTH, intAcc  = iacc} = nth iacc

--defaultApp :: (Applicative f) => Value -> f Value
--defaultApp v = Identity v

--defaultPath :: (AsValue t, Applicative f) => (Value -> f Value) -> t -> f t
--defaultPath defaultApp Null = Null

data ACrdtType = AESONDoc

data ACrdtOps =   ASSIGNAVal {aval :: Value}
                | DELETEAVal
