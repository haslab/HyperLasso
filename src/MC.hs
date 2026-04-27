{-# LANGUAGE OverloadedStrings #-}

module MC where

import Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Identity
import qualified Control.Monad.State as State
import Control.Monad.State (StateT(..))
import Data.Typeable
import Data.Data
import qualified Data.Text as T
import Data.Text (Text(..))
import Shelly
import Safe

import IO
import SMV.NuXmv
import SMV.Trace
import SMV.Syntax
import SMV.Packed as Packed
import SMV.Pretty
import SMV.Typing
import Transform.Pexpr
import Transform.Bexpr
import Transform.Bpacked
import Transform.SMVToSBV
import Transform.Rename
import Utils.Misc
import Utils.Pretty

type CompleteSBVResult = (SBVResult,Bool)

data CompleteMC = NuXmv | Incomplete
    deriving (Data,Typeable,Show,Eq,Enum,Bounded)

data MCResult
    = MCSat Int -- satisfiable with a trace of length
    | MCUnsat -- unsatisfiable
    | MCUnknown -- cannot determine

completeMC :: MonadIO m => Maybe TimeRef -> Bool -> Bool -> CompleteMC -> SBVResult -> MCM m MCResult
completeMC timeRef doRemoveTemps isDebug Incomplete (ty,traces) = return MCUnknown
completeMC timeRef doRemoveTemps isDebug mc (ty,traces) | null (catMaybes traces) = do
    liftIO $ when isDebug $ putStrLn $ "Warning: Skipping complete model checking with no candidate traces"
    return MCUnknown
completeMC timeRef doRemoveTemps isDebug mc (ty,traces) = do
    st <- State.get
    let (_,(bsmvs',bformula')) = normalizeMultiBmodules Map.empty $ fuseOuterTraces (zip traces $ mcMultis st) (mcFormula st)
    case outerQuantBformula bformula' of
        Nothing -> return MCUnsat
        Just _ -> do
            let (smvs',formula') = fromMultiBmodules (bsmvs',bformula')
            doBM Map.empty $ do
                smvs'' <- inBM $ mapM fromPackedBmodule smvs'
                formula'' <- case ty of
                        TRUE -> fromBformula $ normalizeBformula formula'
                        FALSE -> fromBformula $ normalizeBformula $ bnotFormula formula'
                liftIO $ runCompleteMC timeRef doRemoveTemps isDebug mc smvs'' formula''

fuseOuterTraces :: [(Maybe Trace,MultiBmodule)] -> Bformula -> ([MultiBmodule],Bformula)
fuseOuterTraces ((Just tr,smv):smvs) f = fuseOuterTrace tr smv (fuseOuterTraces smvs f)
fuseOuterTraces smvs f = (map snd smvs,f)

fuseOuterTrace :: Trace -> MultiBmodule -> ([MultiBmodule],Bformula) -> ([MultiBmodule],Bformula)
fuseOuterTrace tr smv (smvs,f) = (mapHead fuse smvs,removeOuterQuantBformula f)
    where
    dim = m_dim smv
    vs = Set.unions $ varsBformula f : map varsMultiBmodule smvs
    tr' = filterTrace vs $ addDimTrace dim tr
    fuse bsmv = case traceStateInfo tr' of
        Just info -> addTraceState info tr' smv $ addStateVar info bsmv
        Nothing -> bsmv

runCompleteMC :: Maybe TimeRef -> Bool -> Bool -> CompleteMC -> [PackedPmodule] -> Pformula -> IO MCResult
runCompleteMC timeRef doRemoveTemps isDebug NuXmv smvs0 (vfforalls -> Just (ns,e)) = do
    let smv0 = runIdentity $ transformProduct (zip ns smvs0)
    let smv = normalizePackedPmodule $ smv0 { Packed.p_name = "main", p_ltlspec = normalizeNuXmvExpr $ p_ltlspec smv0 `pimplies` e }
    withTempSMV doRemoveTemps isDebug smv $ \smv' -> do
        ko <- maybeMeasureTimeRef' timeRef $ doCheckLTLSpecNuXMV isDebug smv'
        case ko of
            Nothing -> return MCUnsat
            Just tr -> return $ MCSat $ traceLength tr
runCompleteMC timeRef doRemoveTemps isDebug mc smvs0 formula0 = do
    when isDebug $ putStrLn $ "Warning: Unsupported complete model checking with solver " ++ show mc ++ " for formula " ++ prettyprint formula0
    return MCUnknown


    
-- info for state variable (k,loop)
traceStateInfo :: Trace -> Maybe (Int,Int)
traceStateInfo (trace_states -> sts) = fmap (length sts,) (findIndex state_loop sts)

addStateVar :: (Int,Int) -> MultiBmodule -> MultiBmodule
addStateVar info@(k,l) smv = case Map.lookup v (m_vars smv) of
    Just  _ -> smv
    Nothing -> mapMLTLSpec (\spec -> band spec $ addVarStair Nothing 0 spec) $ addMVars (Map.singleton v kty) smv
  where
    kty = Pint 0 (k-1)
    v = stateVar info
    ve = Bvar v (toVarType kty)
    whenPrev Nothing e = e
    whenPrev (Just prev) e = bg $ prev `bimplies` (bx e)
    addVarStair :: Maybe Bexpr -> Int -> Bexpr -> Bexpr
    addVarStair prev i e = if i >= k
        then band e $ whenPrev prev $ Bop2 Peq ve $ Bint l
        else let now = Bop2 Peq ve $ Bint i in addVarStair (Just now) (succ i) $ band e $ whenPrev prev now
    
stateVar :: (Int,Int) -> Pident
stateVar (k,l) = Pident "state" [Peint k,Peint l]
    
addTraceState :: (Int,Int) -> Trace -> MultiBmodule -> MultiBmodule -> MultiBmodule
addTraceState (k,l) tr smvSrc smvTgt = addMInvar invar $ addMVars varsSrc smvTgt
    where
    statev = Bvar (stateVar (k,l)) (toVarType $ Pint 0 k)
    varsTr :: Set Pident
    varsTr = varsTrace tr
    varsSrc :: PackedBvars 
    varsSrc = Map.filterWithKey (\k _ -> Set.member k varsTr) $ m_vars smvSrc
    invar :: Bexpr
    invar = bandsList $ map invark $ zip [0..] (trace_states tr)
    invark :: (Int,State) -> Bexpr
    invark (i,st) = (Bop2 Peq statev $ Bint i) `bimplies` bandsList (map invarv $ Map.toList $ state_vars st)
    invarv :: (Pident,Pexpr) -> Bexpr
    invarv (var,val) = Bop2 Peq (Bvar var t) (toBexprPure val)
        where t = toVarType $ unsafeLookup var varsSrc


type MCM = StateT MCState

data MCState = MCState
    { mcMultis :: [MultiBmodule]
    , mcFormula :: Bformula
    , mcKs :: Ks
    , mcPrevKs :: PrevKs 
    }

runMCM :: Monad m => [MultiBmodule] -> Bformula -> Ks -> PrevKs -> MCM m a -> m a
runMCM multis formula ks prevks m = flip State.evalStateT (MCState multis formula ks prevks) m

-- negate counter-examples symbolically in the formula and raise k
negateMCFormula :: Monad m => [Maybe Trace] -> (Int -> Int) -> MCM m ()
negateMCFormula trs stepk = do
    st <- State.get
    let tys :: VarTypes
        tys = Map.map toVarType $ Map.unions $ map m_vars $ mcMultis st
    let qs :: [((String,Quant),Bool)]
        qs = outerQuantsBformula $ mcFormula st
    let dims :: [String]
        dims = map (fst . fst) qs
    let qstrs :: [((String,Quant),Maybe Trace)]
        qstrs = map (fst >< id) $ filter (snd . fst) $ zip qs trs
    let outerTrs :: [(String,Trace)]
        outerTrs = catMaybes $ map (\((dim,q),mb) -> fmap (dim,) mb) qstrs
    let outerQuant :: Maybe Quant
        outerQuant = fmap (snd . fst) $ headMay qstrs
    let reK :: Int -> Maybe Trace -> Int
        reK k mbtr = if isJust mbtr then k else stepk k
    let negateTraces :: Bformula -> Maybe Quant -> [(String,Trace)] -> Bformula
        negateTraces f Nothing trs = f
        negateTraces f (Just q) trs = runIdentity $ mapBformula (return . bconstrain q negs) f
            where
            negs = bnot $ bandsList $ map (\(dim,tr) -> traceToBLTL tys $ addDimTrace dim tr) trs
    State.modify $ \st -> st
        { mcKs = joinMap reK (mcKs st) (Map.fromList $ zip dims trs)
        , mcPrevKs = Right $ mcKs st
        , mcFormula = negateTraces (mcFormula st) outerQuant outerTrs
        }

normalizeMC :: MonadIO m => TimeRef -> Bool -> Bool -> MCM m ()
normalizeMC emptyTimeRef doRemoveTemps isDebug = do
    st <- State.get
    (multis',formula') <- liftIO $ splitBformulaMultiBmodule emptyTimeRef doRemoveTemps isDebug (mcMultis st,normalizeBformula $ mcFormula st)
    State.put $ st { mcMultis = multis', mcFormula = formula' }
    
resetFormulaMC :: Monad m => Bformula -> Int -> Int -> MCM m ()
resetFormulaMC formula outerk innerk = do
    let qs = Map.fromList $ map assocr $ outerQuantsBformula formula
    let ks = Map.map (\(q,isOuter) -> if isOuter then outerk else innerk) qs
    State.modify $ \st -> st { mcFormula = formula, mcKs = ks, mcPrevKs = Left $ mcKs st }

normalizeNuXmvFormula :: Pformula -> Pformula
normalizeNuXmvFormula = runIdentity . mapFormula (return . normalizeNuXmvExpr)

normalizeNuXmvExpr :: Pexpr -> Pexpr
normalizeNuXmvExpr e@(Pebool {}) = e
normalizeNuXmvExpr e@(Peint {}) = e
normalizeNuXmvExpr e@(Peident {}) = e
normalizeNuXmvExpr e@(Peop1 Px e1) | typeOfExpr e1 == EInt = Peop1 Pnext (normalizeNuXmvExpr e1)
normalizeNuXmvExpr e@(Peop1 o e1) = Peop1 o (normalizeNuXmvExpr e1)
normalizeNuXmvExpr e@(Peop2 o e1 e2) = Peop2 o (normalizeNuXmvExpr e1) (normalizeNuXmvExpr e2)
normalizeNuXmvExpr e@(Peopn o es) = Peopn o (map normalizeNuXmvExpr es)
normalizeNuXmvExpr e@(Pecase cs) = Pecase (map (normalizeNuXmvExpr >< normalizeNuXmvExpr) cs)
normalizeNuXmvExpr e@(Pedemorgan c e1 e2) = Pedemorgan (normalizeNuXmvExpr c) (normalizeNuXmvExpr e1) (normalizeNuXmvExpr e2)

normalizeNuXmvModule :: PackedPmodule -> PackedPmodule
normalizeNuXmvModule m = m { p_ltlspec = normalizeNuXmvExpr $ p_ltlspec m }


