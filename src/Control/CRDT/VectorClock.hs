
module Control.CRDT.VectorClock where

import Data.CRDT

import Control.Lens

import qualified Data.Map as Map

incLC :: LOGICAL_CLOCK
incLC = LOGICAL_CLOCK 1

newVC :: (Ord nid, Foldable f) => f nid -> VClock nid
newVC fn = foldrOf folded (\k vc0 -> vc0 & at k ?~ mempty) emptyVC fn

newPVC :: (Ord nid, Eq nid, Foldable f) => nid -> f nid -> PVClock nid
newPVC rn fn = foldrOf (folded . filtered (/= rn)) (\k pvc0 -> pvc0 & at k ?~ evc) emptyPVC fn
                 where evc = newVC fn

cmpNextLC :: LOGICAL_CLOCK -> LOGICAL_CLOCK -> Ordering
cmpNextLC rlc mlc = compare mlc (rlc <> incLC)

cmpNextVC :: Ord nid => nid -> VClock nid -> VClock nid -> Maybe Ordering
cmpNextVC mnd rvc mvc = cmpNextLC <$> (rvc ^? ix mnd) <*> (mvc ^? ix mnd)

cmpOrdering :: CVClock -> Ordering -> CVClock
cmpOrdering EQVC  LT = LTVC
cmpOrdering EQVC  EQ = EQVC
cmpOrdering EQVC  GT = GTVC
cmpOrdering LTVC  GT = CONVC
cmpOrdering LTVC  _  = LTVC
cmpOrdering GTVC  LT = CONVC
cmpOrdering GTVC  _  = GTVC
cmpOrdering CONVC _  = CONVC

cmpLC :: LOGICAL_CLOCK -> LOGICAL_CLOCK -> CVClock -> CVClock
cmpLC lc0 lc1 cvc = cmpOrdering cvc (compare lc0 lc1)

cmpVC :: Ord nid => VClock nid -> VClock nid -> Maybe CVClock
cmpVC vc0 vc1 = ifoldrOf ifolded (\ni0 lc0 cvc0 ->
                  (cmpLC lc0) <$> (vc1 ^? ix ni0) <*> cvc0) (pure EQVC) vc0

cmpVCFor :: (Ord nid, Foldable f) => f (Maybe CVClock) -> VClock nid -> VClock nid -> Bool
cmpVCFor fmc vc0 vc1 = elemOf folded (cmpVC vc0 vc1) fmc

nextVC :: Ord nid => nid -> VClock nid -> VClock nid
nextVC nd vc = vc & ix nd <>~ incLC

mergeVC :: (Foldable f, Ord nid, Eq nid) =>
           (LOGICAL_CLOCK -> LOGICAL_CLOCK -> LOGICAL_CLOCK) -> f (VClock nid) -> VClock nid
mergeVC mf vcs = Map.unionsWith mf vcs

mergeMsgVClock :: Ord nid => nid -> VClock nid -> VClock nid -> Maybe (VClock nid)
mergeMsgVClock mnid mvc nvc = do
                                mlc <- mvc ^? ix mnid
                                return (nvc & ix mnid .~ mlc)
