module Data.General where

import Control.Lens
import Data.Semigroup
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

import Data.CRDT
import Control.MCRDT

type IVClock = VClock Int
type ICrdtOps = CrdtOps Int Int
type IMsgOps = MsgOps Int Int Int
type ICrdtData = CrdtData Int Int Int
type ISDPMsgOps = (SDProduct, IMsgOps)
type IARSETData = ARSETData Int Int Int
type IEDFlagData = EDFlagData Int Int Int

cmpVC1 :: Ord nid => VClock nid -> VClock nid -> Maybe CVClock
cmpVC1 vc0 vc1 =
    ifoldrOf ifolded (\ni0 lc0 cvc0 ->
      (cmpLC lc0) <$> (vc1 ^? ix ni0) <*> cvc0) (Just EQVC) vc0

cmpVCFor1 :: (Ord nid, Foldable f) => f (Maybe CVClock) -> VClock nid -> VClock nid -> Bool
cmpVCFor1 fmc vc0 vc1 = elemOf folded (cmpVC vc0 vc1) fmc

mergeMVC :: Ord nid => nid -> VClock nid -> VClock nid -> Maybe (VClock nid)
mergeMVC mnid mvc nvc = do
                          mlc <- mvc ^? ix mnid
                          return (nvc & ix mnid .~ mlc)

intRegMdata :: (VClock Int, MsgData Int Int Int)
intRegMdata = (v020, [msg100, msg200, msg010, msg001, msg201])
                 where v000 = newVC [1,2,3] :: VClock Int
                       v100 = nextVC 1 v000
                       v200 = nextVC 1 v100
                       v201 = nextVC 3 v200
                       v010 = nextVC 2 v000
                       v020 = nextVC 2 v010
                       v001 = nextVC 3 v000
                       o100 = multVal 3 :: CrdtOps Int Int
                       o200 = multVal 5 :: CrdtOps Int Int
                       o001 = multVal 7 :: CrdtOps Int Int
                       o010 = multVal 9 :: CrdtOps Int Int
                       msg100 = MsgOps {_msg_node_id = 1, _msg_vclock =  v100, _crdt_ops = o100} :: MsgOps Int Int Int
                       msg200 = MsgOps {_msg_node_id = 1, _msg_vclock =  v200, _crdt_ops = o200} :: MsgOps Int Int Int
                       msg001 = MsgOps {_msg_node_id = 3, _msg_vclock =  v001, _crdt_ops = o001} :: MsgOps Int Int Int
                       msg010 = MsgOps {_msg_node_id = 2, _msg_vclock =  v010, _crdt_ops = o010} :: MsgOps Int Int Int
                       msg201 = MsgOps {_msg_node_id = 3, _msg_vclock =  v201, _crdt_ops = ResetCRDT} :: MsgOps Int Int Int

resetMdata :: (MsgOps Int Int Int, MsgData Int Int Int)
resetMdata = (msg300, [msg100, msg200, msg010, msg020, msg321])
                where v000 = newVC [1,2,3] :: VClock Int
                      v100 = nextVC 1 v000
                      v200 = nextVC 1 v100
                      v300 = nextVC 1 v200
                      v010 = nextVC 2 v000
                      v020 = nextVC 2 v010
                      v001 = nextVC 3 v000
                      v321 = mergeVC max [v300, v020, v001]
                      o100 = multVal 3 :: CrdtOps Int Int
                      o010 = multVal 3 :: CrdtOps Int Int
                      o321 = multVal 3 :: CrdtOps Int Int
                      msg100 = MsgOps {_msg_node_id = 1, _msg_vclock =  v100, _crdt_ops = o100} :: MsgOps Int Int Int
                      msg010 = MsgOps {_msg_node_id = 2, _msg_vclock =  v010, _crdt_ops = o010} :: MsgOps Int Int Int
                      msg321 = MsgOps {_msg_node_id = 3, _msg_vclock =  v321, _crdt_ops = o321} :: MsgOps Int Int Int
                      msg200 = MsgOps {_msg_node_id = 1, _msg_vclock =  v200, _crdt_ops = ResetCRDT} :: MsgOps Int Int Int
                      msg300 = MsgOps {_msg_node_id = 1, _msg_vclock =  v300, _crdt_ops = ResetCRDT} :: MsgOps Int Int Int
                      msg020 = MsgOps {_msg_node_id = 2, _msg_vclock =  v020, _crdt_ops = ResetCRDT} :: MsgOps Int Int Int

checkResetMsg :: Ord nid => VClock nid -> MsgOps nid k v -> (VClock nid, Bool, Maybe CVClock)
checkResetMsg mvx msg = (vcm, isResetCRDT msg, cmpVC mvx vcm)
                          where vcm = msg ^. msg_vclock

gaddMaybeInt :: Maybe Int -> Maybe Int -> Maybe Int
gaddMaybeInt amv01 amv02 = (+) <$> amv01 <*> amv02

gmultProduct :: Int -> Product Int -> Int
gmultProduct m01 p01 = m01 * (getProduct p01)

gmultMaybeInt :: Maybe Int -> Maybe (Product Int) -> Maybe Int
gmultMaybeInt m01 mp01 = gmultProduct <$> m01 <*> mp01

processIRMsg :: [(SDProduct, MsgOps Int Int Int)]
processIRMsg = [m10, m20, m31, m01, m22]
                 where v00 = newVC [1,2] :: VClock Int
                       v10 = nextVC 1 v00
                       v20 = nextVC 1 v10
                       v01 = nextVC 2 v00
                       v02 = nextVC 2 v01
                       v30 = nextVC 1 v20
                       v31 = mergeVC max [v30, v01]
                       v22 = mergeVC max [v20, v02]
                       c10 = multVal 3 :: CrdtOps Int Int
                       c20 = multVal 4 :: CrdtOps Int Int
                       c31 = addVal 6  :: CrdtOps Int Int
                       c01 = addVal 5  :: CrdtOps Int Int
                       c22 = multVal 7 :: CrdtOps Int Int
                       Just m10 = validateMsg IntReg $ makeMsgOps 1 v10 c10 :: Maybe (SDProduct, MsgOps Int Int Int)
                       Just m20 = validateMsg IntReg $ makeMsgOps 1 v20 c20 :: Maybe (SDProduct, MsgOps Int Int Int)
                       Just m31 = validateMsg IntReg $ makeMsgOps 1 v31 c31 :: Maybe (SDProduct, MsgOps Int Int Int)
                       Just m01 = validateMsg IntReg $ makeMsgOps 2 v01 c01 :: Maybe (SDProduct, MsgOps Int Int Int)
                       Just m22 = validateMsg IntReg $ makeMsgOps 2 v22 c22 :: Maybe (SDProduct, MsgOps Int Int Int)

csvcMsg :: (VClock Int, MsgData Int Int Int)
csvcMsg = (v111, [m100, m101, m111, m200, m211])
            where v000 = newVC [1,2,3] :: VClock Int
                  v100 = nextVC 1 v000
                  v101 = nextVC 3 v100
                  v111 = nextVC 2 v101
                  v200 = nextVC 1 v100
                  v211 = mergeVC max [v200, v111]
                  c000 = multVal 3 :: CrdtOps Int Int
                  m100 = makeMsgOps 1 v100 c000 :: MsgOps Int Int Int
                  m101 = makeMsgOps 1 v101 c000 :: MsgOps Int Int Int
                  m111 = makeMsgOps 1 v111 c000 :: MsgOps Int Int Int
                  m200 = makeMsgOps 1 v200 c000 :: MsgOps Int Int Int
                  m211 = makeMsgOps 1 v211 c000 :: MsgOps Int Int Int

-- m10 m20 m01 m02 m32 m23 m43 m34 m54 m45
-- m01 m02 m10 m20 m23 m32 m34 m43 m45 m54

processReset :: [(SDProduct, IMsgOps)]
processReset = [ma10, mm20, mr32, ma43, ma54, mm01, ma02, mr23, mr34, mm45]
                 where v00 = newVC [1,2] :: IVClock
                       v10 = nextVC 1 v00
                       v20 = nextVC 1 v10
                       v30 = nextVC 1 v20
                       v40 = nextVC 1 v30
                       v50 = nextVC 1 v40
                       v01 = nextVC 2 v00
                       v02 = nextVC 2 v01
                       v03 = nextVC 2 v02
                       v04 = nextVC 2 v03
                       v05 = nextVC 2 v04
                       v32 = mergeVC max [v30, v02]
                       v43 = mergeVC max [v40, v03]
                       v54 = mergeVC max [v50, v04]
                       v23 = mergeVC max [v20, v03]
                       v34 = mergeVC max [v30, v04]
                       v45 = mergeVC max [v40, v05]
                       Just ma10 = validateMsg IntReg $ makeMsgOps 1 v10 $ addVal 2 :: Maybe (SDProduct, IMsgOps)
                       Just mm20 = validateMsg IntReg $ makeMsgOps 1 v20 $ multVal 3 :: Maybe (SDProduct, IMsgOps)
                       Just mr32 = validateMsg IntReg $ makeMsgOps 1 v32 ResetCRDT :: Maybe (SDProduct, IMsgOps)
                       Just ma43 = validateMsg IntReg $ makeMsgOps 1 v43 $ addVal 4 :: Maybe (SDProduct, IMsgOps)
                       Just ma54 = validateMsg IntReg $ makeMsgOps 1 v54 $ addVal 5 :: Maybe (SDProduct, IMsgOps)
                       Just mm01 = validateMsg IntReg $ makeMsgOps 2 v01 $ multVal 6 :: Maybe (SDProduct, IMsgOps)
                       Just ma02 = validateMsg IntReg $ makeMsgOps 2 v02 $ addVal 7 :: Maybe (SDProduct, IMsgOps)
                       Just mr23 = validateMsg IntReg $ makeMsgOps 2 v23 ResetCRDT :: Maybe (SDProduct, IMsgOps)
                       Just mr34 = validateMsg IntReg $ makeMsgOps 2 v34 ResetCRDT :: Maybe (SDProduct, IMsgOps)
                       Just mm45 = validateMsg IntReg $ makeMsgOps 2 v45 $ multVal 8 :: Maybe (SDProduct, IMsgOps)

getResetList :: ([(SDProduct, IMsgOps)], [(SDProduct, IMsgOps)])
getResetList = ([m10, m20, m01, m02, m32, m23, m43, m34, m54, m45],
                [m01, m02, m10, m20, m23, m32, m34, m43, m45, m54])
                 where [m10, m20, m32, m43, m54, m01, m02, m23, m34, m45] = processReset

foldMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
              CrdtType -> CrdtData nid k v -> (SDProduct, MsgOps nid k v) -> Maybe (CrdtData nid k v)
foldMsgOps ctype cdata sdpmsg = processMsgOps ctype sdpmsg cdata

ewFlagData :: [(SDProduct, IMsgOps)]
ewFlagData = [m10, m20, m01, m02]
               where v00 = newVC [1,2] :: IVClock
                     v10 = nextVC 1 v00
                     v20 = nextVC 1 v10
                     v01 = nextVC 2 v00
                     v02 = nextVC 2 v01
                     Just m10 = validateMsg EWFlag $ makeMsgOps 1 v10 Enable  :: Maybe (SDProduct, IMsgOps)
                     Just m20 = validateMsg EWFlag $ makeMsgOps 1 v20 Disable :: Maybe (SDProduct, IMsgOps)
                     Just m01 = validateMsg EWFlag $ makeMsgOps 2 v01 Enable  :: Maybe (SDProduct, IMsgOps)
                     Just m02 = validateMsg EWFlag $ makeMsgOps 2 v02 Disable :: Maybe (SDProduct, IMsgOps)

mint01 :: Int -> Maybe Int
mint01 v = Just v

tmint01 :: Maybe Bool -> Int -> Maybe Int
tmint01 mbv iv = do
  bv <- mbv
  case bv of
    True  -> mint01 iv
    False -> return 0

iemptySet :: Set.Set Int
iemptySet = Set.empty

xinsertIntoSet :: Int -> Int -> Map.Map Int (Set.Set Int) -> Map.Map Int (Set.Set Int)
xinsertIntoSet key val m01 = m01 & at key . non iemptySet %~ Set.insert val

memptySet :: Maybe (Set.Set Int)
memptySet = Just Set.empty

memptyMap :: Map.Map Int (Maybe (Set.Set Int))
memptyMap = Map.empty

minsertData :: [(Int, [Int])]
minsertData = [(1, [10,11]), (2, [20,21])]

minsertIntoSet :: Int -> Int -> Map.Map Int (Maybe (Set.Set Int)) -> Map.Map Int (Maybe (Set.Set Int))
minsertIntoSet key val m01 = m01 & at key . non memptySet %~ (Set.insert <$> Just val <*>)

--finsertIntoSet :: Int -> EDFlag -> Map.Map Int (Maybe EDFlag) -> Map.Map Int (Maybe EDFlag)
--finsertIntoSet key val m01 = m01 & at key . non (pure Disabled) %~ (\val -> pure val)

-- also test RWSet

processARSet :: [(SDProduct, IMsgOps)]
processARSet = [m10, m20, m30, m01, m32, m33, e20, e32]
                  where v00 = newVC [1,2] :: IVClock
                        v10 = nextVC 1 v00
                        v20 = nextVC 1 v10
                        v30 = nextVC 1 v20
                        v01 = nextVC 2 v00
                        v02 = nextVC 2 v01
                        v03 = nextVC 2 v02
                        v32 = mergeVC max [v30, v02]
                        v33 = mergeVC max [v30, v03]
                        Just m10 = validateMsg AWSet $ makeMsgOps  1 v10 $ addElem 1 :: Maybe (SDProduct, IMsgOps)
                        Just m20 = validateMsg AWSet $ makeMsgOps  1 v20 $ addElem 2 :: Maybe (SDProduct, IMsgOps)
                        Just m30 = validateMsg AWSet $ makeMsgOps  1 v30 $ addElem 3 :: Maybe (SDProduct, IMsgOps)
                        Just m01 = validateMsg AWSet $ makeMsgOps  2 v01 $ rmvElem 1 :: Maybe (SDProduct, IMsgOps)
                        Just m32 = validateMsg AWSet $ makeMsgOps  2 v32 $ rmvElem 2 :: Maybe (SDProduct, IMsgOps)
                        Just m33 = validateMsg AWSet $ makeMsgOps  2 v33 $ rmvElem 4 :: Maybe (SDProduct, IMsgOps)
                        Just e20 = validateMsg EWFlag $ makeMsgOps 1 v20 $ Enable    :: Maybe (SDProduct, IMsgOps)
                        Just e32 = validateMsg EWFlag $ makeMsgOps 2 v32 $ Disable   :: Maybe (SDProduct, IMsgOps)

processARSet1 :: [(SDProduct, IMsgOps)]
processARSet1 = [ma10, mr01, mr11]
                  where v00 = newVC [1,2] :: IVClock
                        v10 = nextVC 1 v00
                        v01 = nextVC 2 v00
                        v11 = nextVC 2 v10
                        Just ma10 = validateMsg AWSet $ makeMsgOps  1 v10 $ addElem 1 :: Maybe (SDProduct, IMsgOps)
                        Just mr01 = validateMsg AWSet $ makeMsgOps  2 v01 $ rmvElem 1 :: Maybe (SDProduct, IMsgOps)
                        Just mr11 = validateMsg AWSet $ makeMsgOps  2 v11 $ rmvElem 1 :: Maybe (SDProduct, IMsgOps)

debugEDMsgOps :: (Ord nid, Ord k, Ord v, Show nid, Show k, Show v) =>
                 SDPMsgOps nid k v ->
                 Maybe (EDFlagData nid k v) ->
                 Maybe String
debugEDMsgOps (SDPCheckVC, MsgOps{_msg_vclock=mvc}) medata = do
  traceM ("01:SDPCheckVC:vc:" ++ show mvc ++ " medata:" ++ show medata)
  edata <- medata
  let retval = show (setEDCheckData mvc edata)
  traceM ("02:retval:" ++ retval)
  return retval
debugEDMsgOps _ _ = Nothing

debugASState ::  CrdtType ->
                 (Int, ISDPMsgOps)->
                 IARSETData  ->
                 Maybe IEDFlagData
debugASState ctype (val0, sdpmsg) asstate =
--  asstate & at val0 . non (makeEDFlagData ctype) %~ (updateEDMsgOps sdpmsg)
--  asstate & at val0 ?~ (updateEDMsgOps sdpmsg medata)
--    where medata = asstate ^. at val0 . non (makeEDFlagData ctype)
  updateEDMsgOps sdpmsg (asstate ^. at val0 . non (makeEDFlagData ctype))

debugARSet :: ISDPMsgOps ->
              ICrdtData ->
              Maybe IEDFlagData
debugARSet sdpmsg cdata = do
--  mdata <- getMsgData AWSet cdata
  bctype <- getBaseCRDT AWSet
  (val0, bsdpmsg) <- getBaseMsgOps sdpmsg
  asState <- getASState cdata
  debugASState bctype (val0, bsdpmsg) asState

processMVReg :: [ISDPMsgOps]
processMVReg = [m100, m200, m010, m020, m001, m002]
                 where v000 = newVC [1,2,3] :: IVClock
                       v100 = nextVC 1 v000
                       v200 = nextVC 1 v100
                       v010 = nextVC 2 v000
                       v020 = nextVC 2 v010
                       v001 = nextVC 3 v000
                       v002 = nextVC 3 v001
                       Just m100 = validateMsg MVReg $ makeMsgOps  1 v100 $ assignVal 10 :: Maybe (SDProduct, IMsgOps)
                       Just m200 = validateMsg MVReg $ makeMsgOps  1 v200 $ assignVal 11 :: Maybe (SDProduct, IMsgOps)
                       Just m010 = validateMsg MVReg $ makeMsgOps  2 v010 $ assignVal 20 :: Maybe (SDProduct, IMsgOps)
                       Just m020 = validateMsg MVReg $ makeMsgOps  2 v020 $ assignVal 21 :: Maybe (SDProduct, IMsgOps)
                       Just m001 = validateMsg MVReg $ makeMsgOps  3 v001 $ assignVal 30 :: Maybe (SDProduct, IMsgOps)
                       Just m002 = validateMsg MVReg $ makeMsgOps  3 v002 $ assignVal 31 :: Maybe (SDProduct, IMsgOps)
