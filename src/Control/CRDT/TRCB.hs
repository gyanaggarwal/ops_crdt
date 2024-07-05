{-# LANGUAGE TemplateHaskell #-}

module Control.CRDT.TRCB where

import Control.Lens

import Data.CRDT
import Control.CRDT.VectorClock

data NodeTRCB nid = NodeTRCB {_node_vclock :: VClock nid,
                              _cs_vclock   :: VClock nid,
                              _peer_vclock :: PVClock nid}
                    deriving Show
makeLenses ''NodeTRCB

newNodeTRCB :: (Ord nid, Eq nid, Foldable f) => nid -> f nid -> NodeTRCB nid
newNodeTRCB rn ns = NodeTRCB {_node_vclock = nvc,
                              _cs_vclock   = nvc,
                              _peer_vclock = pvc}
                      where nvc = newVC ns
                            pvc = newPVC rn ns

foldTRCB :: NodeTRCB nid -> [VClock nid]
foldTRCB trcb = [nodeVClock trcb] <> peerVClock trcb ^.. folded

mergeTRCB :: (Ord nid, Eq nid) =>
             (LOGICAL_CLOCK -> LOGICAL_CLOCK -> LOGICAL_CLOCK) -> NodeTRCB nid -> VClock nid
mergeTRCB mf trcb = mergeVC mf $ foldTRCB trcb

setMsgVCTRCB :: (Ord nid, Eq nid) =>
                nid -> VClock nid -> VClock nid -> NodeTRCB nid -> NodeTRCB nid
setMsgVCTRCB mnid mvc nvc trcb = trcb & node_vclock .~ nvc
                                      & peer_vclock . ix mnid .~ mvc

mergeMsgVCTRCB :: (Ord nid, Eq nid) =>
                  nid -> VClock nid -> NodeTRCB nid -> Maybe (NodeTRCB nid)
mergeMsgVCTRCB mnid mvc trcb = do
                                 nvc <- mergeMsgVClock mnid mvc (trcb ^. node_vclock)
                                 return (setMsgVCTRCB mnid (mergeVC max [mvc, trcb ^. peer_vclock . ix mnid]) nvc trcb)

nextVCTRCB :: Ord nid => nid -> NodeTRCB nid -> NodeTRCB nid
nextVCTRCB nid trcb = trcb & node_vclock %~ nextVC nid

nodeVClock :: NodeTRCB nid -> VClock nid
nodeVClock trcb = trcb ^. node_vclock

peerVClock :: NodeTRCB nid -> PVClock nid
peerVClock trcb = trcb ^. peer_vclock
