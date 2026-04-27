module Transform.Bpacked where

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.HashMap.Lazy (HashMap(..))
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Control.Monad.State (State(..),StateT(..))
import qualified Control.Monad.State as State
import Data.HashSet (HashSet(..))
import qualified Data.HashSet as HashSet
import Control.Monad
import qualified Data.Key as K
import Data.List as List
import Data.Hashable
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Data
import Data.Typeable
import Safe

import Utils.Pretty
import SMV.Syntax hiding (p_name)
import SMV.Typing
import SMV.Packed
import SMV.Pretty
import SMV.NuXmv
import Transform.Pexpr
import Transform.Bexpr
import Transform.SMV
import Transform.Substitute
import Utils.Misc

-- we unify the whole model into the LTLSPEC, as the translation to SMT won't make a distinction
data PackedBmodule = PackedBmodule
    { b_name    :: String
    , b_vars    :: PackedBvars
    , b_ltlspec :: Bexpr
    } deriving (Eq,Show)

instance (Pretty PackedBmodule) where
    pretty smv = pretty (runIdentity $ doBMState $ fromPackedBmodule smv)

-- selects only init expressions (may have X)
b_initX :: PackedBmodule -> Bexpr
b_initX bsmv = go btrue (b_ltlspec bsmv)
    where
    go :: Bexpr -> Bexpr -> Bexpr
    go acc (Bopn Pand es) = foldl go acc es
    go acc e = if isFiniteLTLBexpr e then band acc e else acc

-- selects only expressions that have no LTL (except for X)
b_invarX :: PackedBmodule -> Bexpr
b_invarX bsmv = go btrue (b_ltlspec bsmv)
    where
    go :: Bexpr -> Bexpr -> Bexpr
    go acc (Bopn Pand es) = foldl go acc es
    go acc (Bop1 Pg (Bopn Pand es)) = go acc (bands $ HashSet.map bg es) 
    go acc (Bop1 Pg e) | isFiniteLTLBexpr e = band acc e
    go acc e = acc

-- selects only expressions that are suitable for TRANS (G phi, where phi only has X LTL exprs)
-- we actually support X X phi, not exactly just TRANS
b_trans :: PackedBmodule -> Bexpr
b_trans bsmv = go btrue (b_ltlspec bsmv)
    where
    go :: Bexpr -> Bexpr -> Bexpr
    go acc (Bopn Pand es) = foldl go acc es
    go acc (Bop1 Pg (Bopn Pand es)) = go acc (bands $ HashSet.map bg es) 
    go acc (Bop1 Pg e) | isNextLTLBexpr e = band acc e
    go acc e = acc

m_initX :: MultiBmodule -> Bexpr
m_initX bsmv = go btrue (m_ltlspec bsmv)
    where
    go :: Bexpr -> Bexpr -> Bexpr
    go acc (Bopn Pand es) = foldl go acc es
    go acc e = if isFiniteLTLBexpr e then band acc e else acc

m_invarX :: MultiBmodule -> Bexpr
m_invarX bsmv = go btrue (m_ltlspec bsmv)
    where
    go :: Bexpr -> Bexpr -> Bexpr
    go acc (Bopn Pand es) = foldl go acc es
    go acc (Bop1 Pg (Bopn Pand es)) = go acc (bands $ HashSet.map bg es) 
    go acc (Bop1 Pg e) | isFiniteLTLBexpr e = band acc e
    go acc e = acc

bSubstVars :: BSubst -> PackedBvars
bSubstVars = Map.map (fromVarType . varTypeOfBexpr)

dropUnusedBmoduleVars :: Set Pident -> PackedBmodule -> PackedBmodule
dropUnusedBmoduleVars other_used p = p { b_vars = vars' }
    where
    used_vars = Set.unions [bvarSet (b_ltlspec p),other_used]
    vars' = Map.filterWithKey (\n t -> Set.member n used_vars) (b_vars p)

addMLTLSpec :: Bexpr -> MultiBmodule -> MultiBmodule
addMLTLSpec e p = p { m_ltlspec = spec', m_nonempty = False, m_multi = m_multi p || bdimSet e /= Set.singleton (m_dim p) }
    where
    spec' = band e (m_ltlspec p)

mapBLTLSpec :: (Bexpr -> Bexpr) -> PackedBmodule -> PackedBmodule
mapBLTLSpec f p = p { b_ltlspec = f (b_ltlspec p) }

mapMLTLSpec :: (Bexpr -> Bexpr) -> MultiBmodule -> MultiBmodule
mapMLTLSpec f p = p { m_ltlspec = spec', m_nonempty = False, m_multi = bdimSet spec' /= Set.singleton (m_dim p) }
    where
    spec' = f (m_ltlspec p)

addBInvar :: Bexpr -> PackedBmodule -> PackedBmodule
addBInvar e p = p { b_ltlspec = band (bg e) (b_ltlspec p) }
    
addBVars :: PackedBvars -> PackedBmodule -> PackedBmodule
addBVars vs smv = smv { b_vars = Map.union vs (b_vars smv) }
    
addMInvar :: Bexpr -> MultiBmodule -> MultiBmodule
addMInvar e p = addMLTLSpec (bg e) p

addMVars :: PackedBvars -> MultiBmodule -> MultiBmodule
addMVars vs smv = smv { m_vars = Map.union vs (m_vars smv) }
    
type PackedBvars = PackedPvars

type BSubst = Map Pident Bexpr

toBSubst :: Monad m => Subst -> BM m BSubst
toBSubst ss = mapM toBexpr ss

packedBSubst :: PackedBvars -> BSubst
packedBSubst = Map.mapWithKey (\n t -> Bvar n $ toVarType t)

toPackedBmodule :: Monad m => PackedPmodule -> StateT BState m PackedBmodule
toPackedBmodule p0 = transformDeclarative p0 >>= \p -> do
    let name = p_name p
    let vars = p_vars p
    let ss = moduleSubst p
    b <- runBM (Map.map toVarType vars `Map.union` Map.map (const VBool) ss) $ do
        init' <- toBexpr =<< (substExpr ss ss True $ p_init p)
        invar' <- toBexpr =<< (substExpr ss ss True $ p_invar p)
        trans' <- toBexpr =<< (substExpr ss ss True $ p_trans p)
        ltlspec' <- toBexpr =<< (substExpr ss ss True $ p_ltlspec p)
        let spec' = bands $ HashSet.fromList [init',bg invar',bg trans',ltlspec']
        return $ PackedBmodule name vars spec'
    return b

fromPackedBmodule :: Monad m => PackedBmodule -> StateT BState m PackedPmodule
fromPackedBmodule b = do
    let name = b_name b
    let vars = b_vars b
    p <- runBM (Map.map toVarType vars) $ do
        let (init',invar',trans',ltlspec') = splitLTLSpec $ b_ltlspec b
        init'' <- fromBexpr init'
        invar'' <- fromBexpr invar'
        trans'' <- liftM nextFromXExpr $ fromBexpr trans'
        ltlspec'' <- fromBexpr ltlspec'
        return $ PackedPmodule name vars Map.empty init'' invar'' trans'' noPackedPassigns ltlspec''
    return p

splitLTLSpec :: Bexpr -> (Bexpr,Bexpr,Bexpr,Bexpr)
splitLTLSpec ltlspec = (init',invar',trans',ltlspec')
    where
    (init',ltlspec1) = splitInit ltlspec 
    (invar',ltlspec2) = splitInvar ltlspec1 
    (trans',ltlspec') = splitTrans ltlspec2

splitInit e = splitInitAcc e (btrue,btrue)

splitInitAcc :: Bexpr -> (Bexpr,Bexpr) -> (Bexpr,Bexpr)
splitInitAcc (Bopn Pand es) acc = foldl (flip splitInitAcc) acc es
splitInitAcc e (l,r) | not (isLTLBexpr e) = (band e l,r)
splitInitAcc e (l,r) = (l,band e r)

splitInvar e = splitInvarAcc e (btrue,btrue)

splitInvarAcc :: Bexpr -> (Bexpr,Bexpr) -> (Bexpr,Bexpr)
splitInvarAcc (Bopn Pand es) acc = foldl (flip splitInvarAcc) acc es
splitInvarAcc (Bop1 Pg e) (l,r) | not (isLTLBexpr e) = (band e l,r)
splitInvarAcc e (l,r) = (l,band e r)

splitTrans e = splitTransAcc e (btrue,btrue)

splitTransAcc :: Bexpr -> (Bexpr,Bexpr) -> (Bexpr,Bexpr)
splitTransAcc (Bopn Pand es) acc = foldl (flip splitTransAcc) acc es
splitTransAcc (Bop1 Pg e) (l,r) | isTransLTLBexpr e = (band e l,r)
splitTransAcc e (l,r) = (l,band e r)

unsafeFromBSubst :: BSubst -> Subst
unsafeFromBSubst ss = runIdentity $ doBM Map.empty (fromBSubst ss)

-- attempts to substitute definitions inside definitions
fromBSubst :: Monad m => BSubst -> BM m PackedPdefs
fromBSubst biss = do
    foldlWithKeyM register () biss
    foldlWithKeyM add Map.empty biss
  where
    register :: Monad m => () -> Pident -> (Bexpr) -> BM m ()
    register _ n (e) = do
        State.modify $ id >< HashMap.insert e (Peident n $ typeOfBexpr e)
    add :: Monad m => PackedPdefs -> Pident -> Bexpr -> BM m PackedPdefs
    add acc n e = do
        State.modify $ id >< HashMap.delete e -- avoid using the definition on itself
        e' <- fromBexpr e
        State.modify $ id >< HashMap.insert e (Peident n $ typeOfBexpr e) -- restore the definition
        return $ Map.insert n e' acc

fromSubst :: Monad m => Subst -> BM m PackedPdefs
fromSubst ss = toBSubst ss >>= fromBSubst

toHyperBSubst :: String -> BSubst -> BSubst
toHyperBSubst h s = Map.foldrWithKey (\n e -> Map.insert (toHyperPident h n) (toHyperBexpr h e)) Map.empty s

toHyperBexpr :: String -> Bexpr -> Bexpr
toHyperBexpr h e@(Bbool {}) = e
toHyperBexpr h e@(Bints {}) = e
toHyperBexpr h (Bvar n t) = Bvar (toHyperPident h n) t
toHyperBexpr h (Bop1 o e1) = Bop1 o (toHyperBexpr h e1)
toHyperBexpr h (Bop2 o e1 e2) = Bop2 o (toHyperBexpr h e1) (toHyperBexpr h e2)
toHyperBexpr h (Bopn o es) = Bopn o (HashSet.map (toHyperBexpr h) es)

substBformula :: Monad m => [BSubst] -> Bool -> Bformula -> m Bformula
substBformula sss recurse = substBformula' Map.empty sss
    where
    substBformula' :: Monad m => BSubst -> [BSubst] -> Bformula -> m Bformula
    substBformula' acc (ss:sss) (Bexists n f) = liftM (Bexists n) $ substBformula' (Map.union acc $ toHyperBSubst n ss) sss f
    substBformula' acc (ss:sss) (Bforall n f) = liftM (Bforall n) $ substBformula' (Map.union acc $ toHyperBSubst n ss) sss f
    substBformula' acc [] (Bltl e) = liftM Bltl $ substBexpr acc acc recurse e

-- apply a different substitution to left and right expressions (always more substitutions on the right)
substBexpr :: Monad m => BSubst -> BSubst -> Bool -> Bexpr -> m Bexpr
substBexpr mleft mright recurse (Bvar n t) = case Map.lookup n mright of
    Just e -> if recurse then substBexpr mleft mright recurse e else return e
    Nothing -> return $ Bvar n t
substBexpr mleft mright recurse e@(Bbool {}) = return e
substBexpr mleft mright recurse e@(Bints {}) = return e
substBexpr mleft mright recurse (Bop1 o e) = do
    e' <- substBexpr mleft mright recurse e
    return $ Bop1 o e'
substBexpr mleft mright recurse (Bop2 o e1 e2) = do
    e1' <- substBexpr mleft mright recurse e1
    e2' <- substBexpr mleft mright recurse e2
    return $ Bop2 o e1' e2'
substBexpr mleft mright recurse (Bopn o es) = do
    es' <- mapHashSetM (substBexpr mleft mright recurse) es
    return $ bopn o es'

-- a module whose expressions may refer multiple modules
-- all expressions have explicit dimensions, including to the self-module
data MultiBmodule = MultiBmodule
    { m_dim    :: String
    , m_vars    :: PackedBvars
    , m_ltlspec :: Bexpr
    , m_nonempty :: Bool -- if we know that it is not empty
    , m_multi :: Bool -- if it is multi-module (refers variables in other modules)
    } deriving (Eq,Show)

fromMultiBmodules :: ([MultiBmodule],Bformula) -> ([PackedBmodule],Bformula)
fromMultiBmodules ([],f) = ([],f)
fromMultiBmodules (m:ms,f) = (m':ms',f'')
    where
    (m',f'') = fromMultiBmodule (m,f')
    (ms',f') = fromMultiBmodules (ms,f)
    
fromMultiBmodule :: (MultiBmodule,Bformula) -> (PackedBmodule,Bformula)
fromMultiBmodule (MultiBmodule dim vars spec nonempty multi,f) = (PackedBmodule dim vars' specsingle',f')
    where
    (specsingle,specmulti) = splitMulti (Map.keysSet vars) spec
    Just q = outerQuantBformula f
    vars' = mapWithKey remDimPident id vars
    specsingle' = removeDimBexpr specsingle
    f' = runIdentity $ mapBformula (return . bconstrain q specmulti) f

unsafeFromMultiBmodule :: MultiBmodule -> PackedBmodule
unsafeFromMultiBmodule m = fst $ fromMultiBmodule (m,error "no formula")

splitMulti :: Set Pident -> Bexpr -> (Bexpr,Bexpr)
splitMulti vs e = splitMultiAcc vs e (btrue,btrue)
splitMultiAcc :: Set Pident -> Bexpr -> (Bexpr,Bexpr) -> (Bexpr,Bexpr)
splitMultiAcc vs (Bopn Pand es) acc = foldl (flip (splitMultiAcc vs)) acc es
splitMultiAcc vs e (l,r) | bvarSet e `Set.isSubsetOf` vs = (band e l,r)
splitMultiAcc vs e (l,r) = (l,band e r)

toMultiBmodule :: String -> PackedBmodule -> MultiBmodule
toMultiBmodule dim b = MultiBmodule
    { m_dim = dim -- we ignore the module's name and store the hyperformula quantifier's name instead
    , m_vars = mapWithKey addDimIdent id $ b_vars b
    , m_ltlspec = addDimExpr (b_ltlspec b)
    , m_multi = False
    , m_nonempty = False
    } 
  where
    addDimIdent :: Pident -> Pident
    addDimIdent v = addDimPident v $ mkQuantDim dim
    addDimExpr :: Bexpr -> Bexpr
    addDimExpr = normalizeBexpr . mapBexpr r
        where
        r (Bvar v t) = Just $ Bvar (addDimIdent v) t
        r _ = Nothing

-- used non-declared vars
varsMultiBmodule :: MultiBmodule -> Set Pident
varsMultiBmodule m = bvarSet (m_ltlspec m) `Set.difference` (Map.keysSet $ m_vars m)

dimsMultiBmodule :: MultiBmodule -> Set String
dimsMultiBmodule m = bdimSet (m_ltlspec m)

type SplitState = StateT ([(String,(Quant,MultiBmodule))],Set FileHash) IO

-- attempt to move propositions from the formula to the models
-- we are checking that the resulting models are non-empty due to prenex normal form
splitBformulaMultiBmodule :: TimeRef -> Bool -> Bool -> ([MultiBmodule],Bformula) -> IO ([MultiBmodule],Bformula)
splitBformulaMultiBmodule emptyTimeRef doRemoveTemps isDebug (smvs,f) = do
    let qs = quantsBformula f
    let dims = map fst qs
    let e = exprBformula f
    let st0 = (map assocr $ zip qs smvs,Set.empty)
    (e',((unzip . map assocl) -> (qs',smvs'),_)) <- State.runStateT (splitBexpr e) st0
    return (smvs',applyQuantsBexpr qs' e')
  where
    innermostDim :: Bexpr -> SplitState (Maybe String)
    innermostDim e = do
        st <- State.gets fst
        return $ lastMay $ map fst $ filter (\(dim,_) -> Set.member dim $ bdimSet e) st
    
    innerDims :: String -> SplitState [String]
    innerDims dim = do
        st <- State.gets fst
        return $ map fst $ tailSafe $ dropWhile ((/=dim) . fst) st
    
    checkPrenexes :: Quant -> [String] -> SplitState (Maybe Bool)
    checkPrenexes q [] = return (Just True)
    checkPrenexes q (x:xs) = do
        bs <- checkPrenexes q xs
        if bs==Just True then checkPrenex q x else return bs
    
    checkPrenex :: Quant -> String -> SplitState (Maybe Bool)
    checkPrenex Qforall dim = do
        st <- State.gets fst
        case List.lookup dim st of
            Just (Qforall,smv) -> return (Just True)
            Just (Qexists,smv) -> if m_multi smv
                then return (Just False)
                else do
                    ok <- checkNonEmpty dim smv
                    if ok
                        then return (Just True)
                        else return Nothing
            Nothing -> return (Just True)
    checkPrenex Qexists dim = do
        st <- State.gets fst
        case List.lookup dim st of
            Just (Qexists,smv) -> return (Just True)
            Just (Qforall,smv) -> if m_multi smv
                then return (Just False)
                else do
                    ok <- checkNonEmpty dim smv
                    if ok
                        then return (Just True)
                        else return Nothing
            Nothing -> return (Just True)
    
    checkNonEmpty :: String -> MultiBmodule -> SplitState Bool
    checkNonEmpty dim smv = if m_nonempty smv
        then return True
        else do
            (ok,h) <- withSystemTempUnlessError doRemoveTemps isDebug "nonempty.smv" $ \tmpFile -> do
                    let psmv = runIdentity $ doBMState $ fromPackedBmodule $ unsafeFromMultiBmodule smv
                    liftIO $ writeSMV tmpFile (psmv { p_name = "main" })
                    when isDebug $ liftIO $ putStrLn $ "Wrote SMV file " ++ tmpFile ++ " for intermediate model " ++ dim
                    hs <- State.gets snd
                    h <- liftIO $ fileHash tmpFile      
                    ok <- if Set.member h hs
                        then return True
                        else liftIO $ measureTimeRef' emptyTimeRef $ doCheckNonEmptyNuXMV isDebug tmpFile
                    return (ok,h)
            if ok
                then do
                    when isDebug $ liftIO $ putStrLn $ "Intermediate model " ++ dim ++ " is not empty"
                    State.modify $ \(st,hs) -> (modifyAssoc dim (\(q,m) -> (q,m { m_nonempty = True })) st,Set.insert h hs)
                else do
                    when isDebug $ liftIO $ putStrLn $ "Intermediate model " ++ dim ++ " is empty"
                    State.modify $ \(st,hs) -> (takeWhile ((/= dim) . fst) st,hs)
            return ok
    
    splitBexpr :: Bexpr -> SplitState Bexpr
    splitBexpr (Bopn Por es) = splitOr es (\found -> if found then splitBexpr else return)
    splitBexpr (Bopn Pand es) = splitAnd es (\found -> if found then splitBexpr else return)
    splitBexpr e = splitOr (HashSet.singleton e) $ \found e' ->
        if found then splitBexpr e'
        else splitAnd (HashSet.singleton e) (\found -> if found then splitBexpr else return)
        
    splitOr es cont = do
        (e',found) <- liftM (bfor >< id) $ foldM splitForall (HashSet.empty,False) es
        case e' of
            Bopn Por _ -> cont False e'
            otherwise -> cont found e'
    splitAnd es cont = do
        (e',found) <- liftM (bgand >< id) $ foldM splitExists (HashSet.empty,False) es
        case e' of
            Bopn Pand _ -> cont False e'
            otherwise -> cont found e'
    
    splitForall :: (HashSet Bexpr,Bool) -> Bexpr -> SplitState (HashSet Bexpr,Bool)
    splitForall (acc,bool) (Bop1 Pf (Bopn Por es)) = foldM splitForall (acc,bool) (HashSet.map (Bop1 Pf) es)
    splitForall (acc,bool) e = innermostDim e >>= \mb -> case mb of
        Just dim -> do
            st <- State.gets fst
            case List.lookup dim st of
                Just (Qforall,smv) -> do
                    mbok <- innerDims dim >>= \dims -> checkPrenexes Qforall dims
                    case mbok of
                        Nothing -> return (HashSet.empty,True)
                        Just ok -> if ok
                            then do
                                let smv' =  addMLTLSpec (bnot e) smv
                                State.modify $ \(st,hs) -> (updateAssoc (\v _ -> v) dim (Qforall,smv') st,hs)
                                return (acc,True)
                            else return (HashSet.insert e acc,bool)
                otherwise -> return (HashSet.insert e acc,bool)
        Nothing -> return (HashSet.insert e acc,bool)
    
    splitExists :: (HashSet Bexpr,Bool) -> Bexpr -> SplitState (HashSet Bexpr,Bool)
    splitExists (acc,bool) (Bop1 Pg (Bopn Pand es)) = foldM splitExists (acc,bool) (HashSet.map (Bop1 Pg) es)
    splitExists (acc,bool) e = innermostDim e >>= \mb -> case mb of
        Just dim -> do
            st <- State.gets fst
            case List.lookup dim st of
                Just (Qexists,smv) -> do
                    mbok <- innerDims dim >>= \dims -> checkPrenexes Qexists dims
                    case mbok of
                        Nothing -> return (HashSet.empty,True)
                        Just ok -> if ok
                            then do
                                let smv' = addMLTLSpec e smv
                                State.modify $ \(st,hs) -> (updateAssoc (\v _ -> v) dim (Qexists,smv') st,hs)
                                return (acc,True)
                            else return (HashSet.insert e acc,bool)
                otherwise -> return (HashSet.insert e acc,bool)
        Nothing -> return (HashSet.insert e acc,bool)

-- put F phi propositions back together
bfor :: HashSet Bexpr -> Bexpr
bfor es = bors (HashSet.insert (bf $ bors r) l)
    where
    (l,r) = bfor' es

bfor' :: HashSet Bexpr -> (HashSet Bexpr,HashSet Bexpr)
bfor' es = foldl go (HashSet.empty,HashSet.empty) es
    where
    go (l,r) (Bop1 Pf e) = (l,HashSet.insert e r)
    go (l,r) e = (HashSet.insert e l,r)
    
-- put G phi propositions back together
bgand :: HashSet Bexpr -> Bexpr
bgand es = bands (HashSet.insert (bg $ bands r) l)
    where
    (l,r) = bgand' es

bgand' :: HashSet Bexpr -> (HashSet Bexpr,HashSet Bexpr)
bgand' es = foldl go (HashSet.empty,HashSet.empty) es
    where
    go (l,r) (Bop1 Pg e) = (l,HashSet.insert e r)
    go (l,r) e = (HashSet.insert e l,r)
    
joinLTL :: Bexpr -> Bexpr
joinLTL (Bopn Pand es) = if HashSet.null r
    then bands (HashSet.map joinLTL l)
    else bands (HashSet.insert (bg $ joinLTL $ bands r) $ HashSet.map joinLTL l)
  where (l,r) = bgand' es
joinLTL (Bopn Por es) = if HashSet.null r
    then bors (HashSet.map joinLTL l)
    else bors (HashSet.insert (bf $ joinLTL $ bors r) $ HashSet.map joinLTL l)
  where (l,r) = bfor' es
joinLTL (Bop1 o e1) = Bop1 o (joinLTL e1)
joinLTL (Bop2 o e1 e2) = Bop2 o (joinLTL e1) (joinLTL e2)
joinLTL e = e

-- groups a ltlspec by dimensions
groupLTL :: Bexpr -> Map (Set String) Bexpr
groupLTL (Bopn Pand es) = Map.map joinLTL $ foldl go Map.empty es
    where
    go acc e = Map.insertWith band (bdimSet e) e acc
groupLTL e = Map.singleton (bdimSet e) e


type AndExprs = Map (Set Pident) Bexpr

mkAndExprs :: [Bexpr] -> AndExprs
mkAndExprs [] = Map.empty
mkAndExprs (e:es) = Map.insertWith band (bvarSet e) e (mkAndExprs es)


andAndExprs :: AndExprs -> AndExprs -> AndExprs
andAndExprs xs ys = Map.unionWith band xs ys
