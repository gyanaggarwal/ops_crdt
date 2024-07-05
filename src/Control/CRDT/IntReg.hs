module Control.CRDT.IntReg where

import Control.Lens
import Data.Semigroup

import Data.CRDT
import Control.CRDT.MsgData

addVal :: TIntVal -> CrdtOps k v
addVal a = ADDVal $ AddVal {_addval = a}

multVal :: TIntVal -> CrdtOps k v
multVal m = MULTVal $ MultVal {_multval = Product m}

memptyProduct :: Maybe (Product TIntVal)
memptyProduct = pure (Product 1)

multIntProduct :: TIntVal -> Product TIntVal -> TIntVal
multIntProduct v01 pv02 = v01 * getProduct pv02

appMultIntProduct :: Maybe TIntVal -> Maybe (Product TIntVal) -> Maybe TIntVal
appMultIntProduct mi01 mpi01 = multIntProduct <$> mi01 <*> mpi01

getMultVal :: MsgOps nid k v -> Maybe (Product TIntVal)
getMultVal msg = do
                   mv01 <- msg ^. crdt_ops ^? _MULTVal
                   return (mv01 ^. multval)

getAddVal :: MsgOps nid k v -> Maybe TIntVal
getAddVal msg = do
                  av01 <- msg ^. crdt_ops ^? _ADDVal
                  return (av01 ^. addval)

getIRState :: CrdtData nid k v -> Maybe TIntVal
getIRState cdata = do
                     irData <- cdata ^? _IRData
                     return (irData ^. intRegData)

getIRMsgData :: CrdtData nid k v -> Maybe (MsgData nid k v)
getIRMsgData cdata = do
                       irdata <- cdata ^? _IRData
                       return (irdata ^. irgMsgData)

setIRMsgData :: MsgData nid k v -> CrdtData nid k v -> CrdtData nid k v
setIRMsgData mdata cdata = cdata & _IRData . irgMsgData .~ mdata

setIRData :: Maybe TIntVal -> MsgData nid k v -> CrdtData nid k v -> Maybe (CrdtData nid k v)
setIRData mirv mdata cdata = do
                               irv <- mirv
                               return $ cdata & _IRData . irgMsgData .~ mdata & _IRData . intRegData .~ irv

combineMultVal :: Ord nid => VClock nid -> MsgData nid k v -> Maybe (Product TIntVal)
combineMultVal mvc mdata = foldrOf folded (<>) memptyProduct
                           $ (filterNotResetCONMsgData mvc mdata) ^.. folded . to getMultVal

queryIntReg :: Maybe (CrdtData nid k v) -> Maybe TIntVal
queryIntReg mcdata = do
                       cdata <- mcdata
                       getIRState cdata
