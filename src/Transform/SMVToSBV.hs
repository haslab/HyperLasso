{-# LANGUAGE GADTs, BangPatterns, DeriveGeneric #-}

module Transform.SMVToSBV 
    ( module Transform.SMVToSBV
    , module SMT.SBV
    ) where
        
import Data.Foldable
import Control.Exception
import Control.DeepSeq
import Data.Data
import GHC.Generics
import Data.Hashable
import Data.Maybe
import Data.Set (Set(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map(..))
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap(..))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import qualified Control.Monad.State as State
import Data.Proxy
import Data.HashSet (HashSet(..))
import qualified Data.HashSet as HashSet
import Data.List.Split
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Safe
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IntSet
import Control.Monad.Reader (ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.IORef as IORef
import Control.Concurrent.MVar as MVar
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.String as Parsec
import Prettyprinter

import SMT.SBV
import Transform.Substitute
import Transform.Pexpr
import Transform.Bexpr
import Transform.Bpacked
import SMV.Syntax
import SMV.Trace
import SMV.Pretty
import SMV.Parser
import SMV.Typing
import Utils.Misc
import Utils.Pretty
import Utils.Parser

data ResultType = TRUE | FALSE
    deriving (Data,Typeable,Eq,Ord,Show,Generic)

instance NFData ResultType
    
instance Pretty ResultType where
    pretty TRUE = pretty "TRUE"
    pretty FALSE = pretty "FALSE"

resultTypeParser :: Parser ResultType
resultTypeParser = (Parsec.string "TRUE" >> return TRUE) <||> (Parsec.string "FALSE" >> return FALSE)

smtConfig :: Bool -> Solver -> SMTConfig
smtConfig isDebug s = smtDebug isDebug $ smtCfg s

type SBVSt = StateT SBVState IO

newtype SBVM a = SBVM { unSBVM :: ReaderT (IORef SBVState) Symbolic a }
    deriving (Functor,Applicative,Monad)

instance MonadIO SBVM where
    liftIO = SBVM . liftIO

instance MonadState SBVState SBVM where
    get = SBVM $ do
        (ref) <- Reader.ask
        liftIO $ readIORef ref
    put s' = SBVM $ do
        (ref) <- Reader.ask
        liftIO $ writeIORef ref $! s'
    state f = SBVM $ do
        (ref) <- Reader.ask
        liftIO $ atomicModifyIORef ref (swap . f)
    
evalSBVSt_ :: SBVSt a -> IO a
evalSBVSt_ m = State.evalStateT m emptySBVState

runSBVM :: SBVNFData a => SBVM a -> SBVState -> Symbolic (a,SBVState)
runSBVM (SBVM m) st = do
    ref <- liftIO $ newIORef st
    a <- Reader.runReaderT m (ref)
    a' <- forceSBV a
    s <- liftIO $ readIORef ref
    return (a',s)

getSBVM :: SBVM (IORef SBVState)
getSBVM = SBVM $ Reader.ask

evalSBVMWith :: (IORef SBVState) -> SBVM a -> Symbolic a
evalSBVMWith st (SBVM m) = runReaderT m st

type SFunExpr = Either SFun SExpr

data SBVState = SBVState
    { sbvVars :: Map Pident SFunExpr -- a function or a value
    , sbvCache :: Map CacheCtx (HashMap Bexpr SExpr)
    , sbvIgnores :: Set String -- variables for the solver to ignore when generating witnesses
    , sbvNextName :: Int -- unique indices for variable generation (must be careful not to clash with SBV-generated variables)
    , sbvDefinedLTLFuns :: HashMap Bexpr DefinedLTLFun
    , sbvUnrolled :: UnrolledExprs -- unrolled facts over indexed variables, to avoid quantifying fixed variables
    , sbvConsts :: BSubst -- indexed variables that are not quantified (duplicated in sbvVars, but we keep this for the final example/counter-example trace)
    }

-- a context (when generating expressions for a model, since variables differ)
-- Nothing = global
-- Just = relative to a specific index
type CacheCtx = Maybe (Int,Maybe KnownLoops)

-- keep only non-SBV data
resetSBV :: Monad m => StateT SBVState m ()
resetSBV = State.modify $ \st -> emptySBVState 

nextName :: String -> SBVM String
nextName prefix = State.state $ \st ->
    let name = prefix ++ show (sbvNextName st)
        st' = st { sbvNextName = succ $ sbvNextName st }
    in (name,st')

addSBVIgnore :: String -> SBVState -> SBVState
addSBVIgnore s st = st { sbvIgnores = Set.insert s $ Set.insert bars $ sbvIgnores st }
    where
    -- SBV sometimes puts bars around variable names
    bars = "|"++s++"|"

getSBVCache :: CacheCtx -> Bexpr -> SBVState -> Maybe SExpr
getSBVCache ctx e st = do
    h <- Map.lookup ctx (sbvCache st)
    HashMap.lookup e h

addSBVCache :: CacheCtx -> Bexpr -> SExpr -> SBVState -> SBVState
addSBVCache ctx e sv st = st { sbvCache = Map.insertWith HashMap.union ctx (HashMap.singleton e sv) (sbvCache st) }

emptySBVState :: SBVState
emptySBVState = SBVState Map.empty Map.empty Set.empty 0 HashMap.empty Map.empty Map.empty

-- (SAT / UNSAT,witnesses)
type SBVResult = (ResultType,[Maybe Trace])

type SMTSubst = Map String (IntMap Subst,Subst)

emptySMTSubst :: SMTSubst
emptySMTSubst = Map.empty 

mkPidentDimI :: Pident -> String -> Maybe Int -> Pident
mkPidentDimI n dim Nothing = addDimPident n (mkQuantDim dim)
mkPidentDimI n dim (Just i) = addDimPident (mkPidentDimI n dim Nothing) (Peint i)

splitPidentDimI :: Pident -> Maybe (Pident,String,Maybe Int)
splitPidentDimI (Pident n [Peident (Pident dim []) EUnknown,Peint i]) = Just (Pident n [],dim,Just i)
splitPidentDimI (Pident n [Peident (Pident dim []) EUnknown]) = Just (Pident n [],dim,Nothing)
splitPidentDimI _ = Nothing

corePidentDimI :: Pident -> (Pident,String)
corePidentDimI n = case splitPidentDimI n of
    Just (n,dim,_) -> (n,dim)
    otherwise -> error $ "corePidentDimI " ++ show n

constructSBVResult :: Ks -> Bool -> [(String,Quant)] -> SBVState -> SatResult -> SBVResult
constructSBVResult ks isNegated dims st (SatResult res) = case res of
    Unsatisfiable _ _ -> (if isNegated then TRUE else FALSE,map (const Nothing) dims)
    Satisfiable _ model -> (if isNegated then FALSE else TRUE,map mkDim dims)
        where
        mkSubst :: [(String,CV)] -> Subst
        mkSubst cvs = selfSubst $ foldl go (unsafeFromBSubst $ sbvConsts st) cvs
            where
            go :: Subst -> (String,CV) -> Subst
            go acc (str,cv) = case parsePident str of
                Nothing -> acc
                Just pn -> Map.insert pn (cvToExpr cv) acc
        subst = mkSubst (modelAssocs model)
        defs = mkDefs (Map.toList subst)
        mkDefs :: [(Pident,Pexpr)] -> SMTSubst
        mkDefs ss = foldl (\acc (pn,e) -> insertPident acc pn e) emptySMTSubst ss
        insertPident :: SMTSubst -> Pident -> Pexpr -> SMTSubst
        insertPident acc pn val = case splitPidentDimI pn of
            Nothing -> acc
            Just (n,dim,Just i) -> insert dim (IntMap.singleton i $ Map.singleton n val,Map.empty) acc
            Just (n,dim,Nothing) -> insert dim (IntMap.empty,Map.singleton n val) acc
        insert :: String -> (IntMap Subst,Subst) -> SMTSubst -> SMTSubst
        insert dim x acc = Map.insertWith (\(x1,y1) (x2,y2) -> (IntMap.unionWith mapUnionError x1 x2,mapUnionError y1 y2)) dim x acc
        mkDim :: (String,Quant) -> Maybe Trace
        mkDim (dim,q) = case Map.lookup dim defs of
            Nothing -> Nothing
            Just is -> mkTrace dim q is
        mkTrace :: String -> Quant -> (IntMap Subst,Subst) -> Maybe Trace
        mkTrace dim q (is,loopss) = case IntMap.lookupMax is of
            Nothing -> Nothing
            Just (maxi,_) -> do
                let getLoop = case Map.lookup loopVar loopss of
                        Just (Peint i) -> Just i
                        Nothing -> Nothing
                let ty = if isNegated then Counterexample else Example
                let mkState i = 
                        let sts = (fromMaybe Map.empty $ IntMap.lookup i is)
                        in State (show i) (Just i==getLoop) sts 
                let sts = map mkState [0..maxi]
                return $ Trace (prettyprint q ++ " " ++ dim) ty sts
    otherwise -> error $ "unexpected SMT result " ++ if isNegated then show (SatResult res) else show (ThmResult res)

transformBsToSBV :: Maybe TimeRef -> SMTConfig -> Ks -> PrevKs -> [MultiBmodule] -> Bformula -> Bool -> SBVSt SBVResult
transformBsToSBV timeRef cfg ks prevks bsmvs formula witness = do
    let (formula',isNegated) = skolemBformula formula
    let qs = outerQuantsBformula formula'
    let dims = map (fst . fst) qs
    let areOuter = all snd qs -- when all quantifiers are outer
    let qqs = map (snd >< id) qs
    
    -- TODO: we could try to estimate minimal lassos from fairness constraints; default value here
    let kLassos = Map.map (,1) ks
    
    let prop = do
            modelsToSBVSym kLassos prevks bsmvs qs $ do
                ltlToSBool areOuter kLassos (exprBformula formula')
    res <- satWithSBVM timeRef cfg prop
    st <- State.get
    let sbvres = constructSBVResult ks isNegated (quantsBformula formula) st res
    return sbvres
        
preprocessLTL :: Bexpr -> SBVM (Map (Set String) Bexpr)
preprocessLTL e = do
    let es = groupLTL e
    return es
    
ltlToSBool :: Bool -> KLassos -> Bexpr -> SBVM SExpr
ltlToSBool areOuter kLassos ltl = do
    es <- preprocessLTL ltl
    liftM andsSBV $ mapM go (Map.toList es)
  where
    go (dims,ltl) = ltlToSBoolConcrete areOuter kLassos renderPident ltl

loopVar :: Pident
loopVar = Pident "loop" []

modelsToSBVSym :: KLassos -> PrevKs -> [MultiBmodule] -> [((String,Quant),Bool)] -> SBVM SExpr -> SBVM SExpr
modelsToSBVSym kLassos prevks [] [] c = c
modelsToSBVSym kLassos prevks (bsmv:bsmvs) ((q,True):qs) c = do
    modelToSBV kLassos prevks bsmv (q,True) $ modelsToSBVSym kLassos prevks bsmvs qs c
modelsToSBVSym kLassos prevks bsmvs qs c = do
    mapSymbolic quantifiedSExpr $ modelsToSBVPure kLassos prevks bsmvs qs c
modelsToSBVSym kLassos prevks bsmvs qs c = do
    mapSymbolic quantifiedSExpr $ modelsToSBVPure kLassos prevks bsmvs qs c

modelsToSBVPure :: KLassos -> PrevKs -> [MultiBmodule] -> [((String,Quant),Bool)] -> SBVM SExpr -> SBVM SExpr
modelsToSBVPure kLassos prevks [] [] c = c
modelsToSBVPure kLassos prevks (bsmv:bsmvs) ((q,False):qs) c = do
    modelToSBV kLassos prevks bsmv (q,False) $ modelsToSBVPure kLassos prevks bsmvs qs c

modelToSBV :: KLassos -> PrevKs -> MultiBmodule -> ((String,Quant),Bool) -> SBVM SExpr -> SBVM SExpr
modelToSBV kLassos prevks bsmv ((dim,q),isOuter) c = do
    modelToSBVVars kLassos prevks bsmv ((dim,q),isOuter) $ do
        bm <- modelToSBVBody kLassos bsmv ((dim,q),isOuter)
        constrainSBV (q,isOuter) bm $ c

sbvKindWith :: VarType -> Kind
sbvKindWith VBool = KBool
sbvKindWith t = sbvKind t

modelToSBVVars :: KLassos -> PrevKs -> MultiBmodule -> ((String,Quant),Bool) -> SBVM SExpr -> SBVM SExpr
modelToSBVVars kLassos prevks bsmv ((dim,q),isOuter) c = do
    let (k,minLasso) = unsafeLookup dim kLassos
    let prevk :: Maybe Int = either (const Nothing) (Map.lookup dim) prevks
    let kindk = sbvKindWith $ unrollTy k
    
    addUnrolledAssigns k prevk bsmv
    
    -- declare loop variable
    let loopVarName = addDimPident loopVar $ mkQuantDim dim
    let kindLoop = sbvKindWith (unrollTy $ k - minLasso + 1)

    quantifyVar isOuter loopVarName (unrollTy $ k - minLasso + 1) q 
        
    -- declare loop variable bounds
    loopV <- getLoopVar dim
    let loopBounds = compareSBV Pleq (intSBV kindLoop 0) loopV `andSBV` compareSBV Pleq loopV (intSBV kindLoop $ k-minLasso)
    
    -- create variables (we only pack indices of the same variable, but could pack more. this seems heuristically better)
    -- we only pack inner quantified variables
    quantvars <- liftM concat $ forM (Map.toList $ m_vars $ bsmv) $ \(v,t) -> do
        let vt = toVarType t
        let kind = sbvKindWith vt
        liftM (concatQuantifiedVarLazy . catMaybes) $ forM [0..k-1] $ \i -> do
            -- declare unrolled variables
            let namei = addDimPident v (Peint i)
            assign <- findUnrolledAssigns namei k
            quantifyMultiLazy isOuter kLassos namei vt q kindk assign
    quantifyLazyVars quantvars
        
    -- apply variable bounds
    varsBounds <- liftM concat $ forM [0..k-1] $ \i -> do
        liftM concat $ forM (Map.toList $ m_vars $ bsmv) $ \(v,t) -> do
            let vt = toVarType t
            let kind = sbvKindWith vt
            let namei = addDimPident v (Peint i)
            -- declare unrolled variable bounds
            vi <- liftM unRight $ getVar namei
            case vt of
                VBool -> return []
                VInt ts -> return [mkIntRangeSBool vi (Just ts) (enumerateKind kind)]
    
    let restrictions = andsSBV $ varsBounds ++ [loopBounds]
    constrainSBV (q,isOuter) restrictions $ c  

-- even though indices only go up to k-1, we actually need k since we use it to store the lasso length
unrollTy :: Int -> VarType
unrollTy k = VInt $ IntSet.fromList [0..k]

quantifyArray :: Pident -> Kind -> Kind -> Quant -> SBVM SExpr
quantifyArray v kind kindk q = quantifyVarPure True v (KArray kindk kind) q

-- bitpack quantified variables
-- we only keep pure vars lazily
quantifyLazyVars :: QuantifiedVarsLazy -> SBVM ()
quantifyLazyVars qs = mapM_ (goQuant) qs
    where
    goQuant :: (Quant,Map Kind (Set Pident)) -> SBVM ()
    goQuant (q,vs) = mapM_ (goKind q . (id >< Set.toList)) (Map.toList vs)
    
    goKind :: Quant -> (Kind,[Pident]) -> SBVM ()
    goKind q (k,[]) = return ()
    goKind q (k,[n]) = quantifyVarPure True n k q >> return ()
    goKind q (k,ns) = do
        let (k',projs) = bitpackSBV k (length ns)
        packedStr <- nextName "packed"
        let packedName = Pident packedStr []
        packedVar <- quantifyVarPure False packedName k' q
        forM_ (zip ns projs) $ \(n,proj) -> do
            letSBVM n k $ return $ proj packedVar
            
type QuantifiedVarLazy = (Pident,Kind,Quant)

type QuantifiedVarsLazy = [(Quant,Map Kind (Set Pident))]

singletonQuantifiedVarsLazy :: QuantifiedVarLazy -> QuantifiedVarsLazy
singletonQuantifiedVarsLazy (n,k,q) = [(q,Map.singleton k $ Set.singleton n)]

addQuantifiedVarLazy :: QuantifiedVarLazy -> QuantifiedVarsLazy -> QuantifiedVarsLazy
addQuantifiedVarLazy x [] = singletonQuantifiedVarsLazy x
addQuantifiedVarLazy x@(nx,kx,qx) (y@(qy,my):ys) = if qx==qy
    then (qy,Map.insertWith Set.union kx (Set.singleton nx) my):ys
    else singletonQuantifiedVarsLazy x ++ (y:ys)

concatQuantifiedVarLazy :: [QuantifiedVarLazy] -> QuantifiedVarsLazy
concatQuantifiedVarLazy [] = []
concatQuantifiedVarLazy (x:xs) = addQuantifiedVarLazy x (concatQuantifiedVarLazy xs)

-- delayed quantification so that we can pack variables before actual quantification in SBV
-- try to avoid quantifying variables whose value is actually determined by previous variables
quantifyMultiLazy :: Bool -> KLassos -> Pident -> VarType -> Quant -> Kind -> (Maybe Bexpr) -> SBVM (Maybe QuantifiedVarLazy)
quantifyMultiLazy isOuter kLassos namei ty q kindi Nothing = do
    quantifyVarLazy isOuter namei ty q
quantifyMultiLazy isOuter kLassos namei ty q kindi (Just assign) = do
    registerConst namei assign
    let kind = sbvKindWith ty
    letSBVM namei kind $ exprToSBVMulti isOuter kind kLassos kindi assign
    return Nothing

addUnrolledAssigns :: Int -> Maybe Int -> MultiBmodule -> SBVM ()
addUnrolledAssigns k (maybe 0 id -> prevk) bsmv = do
    State.modify $ \st -> st { sbvUnrolled = add (sbvConsts st) (sbvUnrolled st) }
  where
    inits' = replaceNextI 0 $ m_initX bsmv
    invars = m_invarX bsmv
    invars' = map (\i -> replaceNextI i invars) [0..k-1]
    
    add :: BSubst -> UnrolledExprs -> UnrolledExprs
    add ss c = c `andAndExprs` normalizeUnrolleds ss (mkAndExprs $ concatMap unbands $ inits':invars')

type UnrolledExprs = AndExprs

-- converts an expr to an expr with explicit variable indices
-- replaces X with concrete indices    
replaceNextI :: Int -> Bexpr -> Bexpr
replaceNextI inc e@(Bints _) = e
replaceNextI inc e@(Bbool _) = e
replaceNextI inc (Bvar (splitPidentDimI -> Just (n,dim,mbi)) t) = case mbi of
    Nothing -> Bvar (mkPidentDimI n dim (Just inc)) t
    Just i  -> Bvar (mkPidentDimI n dim (Just $ inc+i)) t
replaceNextI inc (Bopn o es) = bopn o $ HashSet.map (replaceNextI inc) es
replaceNextI inc (Bop1 Px e1) = replaceNextI (succ inc) e1
replaceNextI inc (Bop1 o e1) = Bop1 o (replaceNextI inc e1)
replaceNextI inc (Bop2 o e1 e2) = Bop2 o (replaceNextI inc e1) (replaceNextI inc e2)
replaceNextI inc ltl = error $ "replaceNextI: " ++ show ltl

dropVarIndex :: Pident -> Pident
dropVarIndex pn@(Pident n dims) = case last dims of
    Peint i -> Pident n (init dims)
    otherwise -> pn

varIndex :: Pident -> Maybe Int
varIndex pn@(Pident n dims) = case last dims of
    Peint i -> Just i
    otherwise -> Nothing

findUnrolledAssigns :: Pident -> Int -> SBVM (Maybe Bexpr)
findUnrolledAssigns namei k = State.state getAss
    where
    
    getAss st = 
        let e = sbvUnrolled st
            defs = Map.keysSet $ sbvVars st
        in case findAssignUnrolledExpr namei k defs e of
            Nothing -> (Nothing,st)
            Just (assign,e') ->
                let st' = st { sbvUnrolled = e' }
                in (Just assign,st')

substUnrolleds :: BSubst -> UnrolledExprs -> UnrolledExprs
substUnrolleds ss es = Map.mapWithKey go es
    where
    go k e = if Set.null (Set.intersection k $ Map.keysSet ss)
        then e
        else normalizeUnrolled ss e

normalizeUnrolleds :: BSubst -> UnrolledExprs -> UnrolledExprs
normalizeUnrolleds ss es = Map.map (normalizeUnrolled ss) es

normalizeUnrolled :: BSubst -> Bexpr -> Bexpr
normalizeUnrolled ss e = normalizeBexpr $ runIdentity $ substBexpr ss ss True e

coreSetIn :: Bexpr -> Int -> Set Pident -> Bool
coreSetIn e k defs = ok && withink
    where
    lefts = Set.difference (bvarSet e) defs
    ok = Set.isSubsetOf (Set.map dropVarIndex lefts) defs
    withink = all (<= k) $ catMaybes $ map varIndex (Set.toList lefts)

findAssignUnrolledExpr :: Pident -> Int -> Set Pident -> UnrolledExprs -> Maybe (Bexpr,UnrolledExprs)
findAssignUnrolledExpr name k defs es = find (Map.toList es)
    where
    find [] = Nothing
    find ((i,e):rec) = if Set.null (Set.intersection (Set.singleton name) i)
        then find rec
        else do
            let found = do
                    (assign) <- findAssignExpr name k defs e 
                    let ss = Map.singleton name assign
                    return (assign,substUnrolleds ss es)
            let notfound = find rec 
            found `mplus` notfound

findAssignExpr :: Pident -> Int -> Set Pident -> Bexpr -> Maybe Bexpr
findAssignExpr name k defs e@(vDetIntVar name -> Just i) = Just $ Bints $ IntSet.singleton i
findAssignExpr name k defs (Bop2 Pequiv (Bvar v VBool) e2) | v == name && coreSetIn e2 k defs = Just e2
findAssignExpr name k defs (Bop2 Pequiv e1 (Bvar v VBool)) | v == name && coreSetIn e1 k defs = Just e1
findAssignExpr name k defs (Bop2 Peq (Bvar v _) e2) | v == name && coreSetIn e2 k defs = Just e2
findAssignExpr name k defs (Bop2 Peq e1 (Bvar v _)) | v == name && coreSetIn e1 k defs = Just e1
findAssignExpr name k defs (Bopn Pand es) = go (HashSet.toList es)
    where
    go [] = Nothing
    go (e:es) = (findAssignExpr name k defs e) `mplus` (go es)
findAssignExpr name k defs e = Nothing

vDetIntVar :: Pident -> Bexpr -> Maybe Int
vDetIntVar name e = case collectIntVar name e of
    Just is -> isSingletonIntSet is
    Nothing -> Nothing

collectIntVar :: Pident -> Bexpr -> Maybe IntSet
collectIntVar name (vbvarin -> Just ((==name) -> True,t,is)) = Just is
collectIntVar name (Bopn o es) = join o $ map (collectIntVar name) $ HashSet.toList es
  where
    join Por [] = Nothing
    join Por [Nothing] = Nothing
    join Por [Just y] = Just y
    join Por (Nothing:ys) = Nothing
    join Por (Just y:ys) = fmap (IntSet.union y) (join Por ys)
    join Pand xs = case catMaybes xs of
        [] -> Nothing
        ys -> Just $ intersectionsIntSet ys
    join o xs = Nothing
collectIntVar name e = Nothing
    
quantifyVarLazy :: Bool -> Pident -> VarType -> Quant -> SBVM (Maybe QuantifiedVarLazy)
quantifyVarLazy isOuter n t q = do
    let k = sbvKindWith t
    case t of
        VInt (isSingletonIntSet -> Just i) -> do
            registerConst n $ Bint i
            letSBVM n k $ return $ intSBV k i
            return Nothing
        otherwise -> do
            if isOuter
                then do
                    quantifyVarSym n k q
                    return Nothing
                else do
                    return $ Just (n,k,q)
            
quantifyVar :: Bool -> Pident -> VarType -> Quant -> SBVM ()
quantifyVar isOuter n t q = do
    let k = sbvKindWith t
    case t of
        VInt (isSingletonIntSet -> Just i) -> do
            letSBVM n k $ return $ intSBV k i
        otherwise -> do
            if isOuter then quantifyVarSym n k q else quantifyVarPure True n k q
            return ()
    
quantifyVarSym :: Pident -> Kind -> Quant -> SBVM SExpr
quantifyVarSym n t q = do
    x <- liftM fromSVal $ SBVM $ lift $ freeVar (prettyPident n) t
    registerVar n (Right x)
    return x
    
quantifyVarPure :: Bool -> Pident -> Kind -> Quant -> SBVM SExpr
quantifyVarPure doRegister n t q = do
    x <- liftM fromSVal $ SBVM $ lift $ quantifiedVar q t
    when doRegister $ registerVar n (Right x)
    return x

getLoopVar :: String -> SBVM SExpr
getLoopVar dim = liftM unRight $ getVar (addDimPident loopVar $ mkQuantDim dim)

declareMultiplexer :: Bool -> String -> Kind -> [Int] -> (Int -> SBVM SExpr) -> Kind -> SBVM (SExpr -> SExpr)
declareMultiplexer isOuter sbv_name kind domain applydomain kinddomain = do
    State.modify $ addSBVIgnore sbv_name
    if isOuter
        then do
            let fList = uninterpretedFun sbv_name [kinddomain] kind
            let f x = fromSVal $ fList [toSVal x]
            forM domain $ \i -> do
                vi <- applydomain i
                constrainSBV_ $ compareSBV Peq (f $ intSBV kinddomain i) vi
            return f
        else do
            xs <- mapM applydomain domain
            let f x = iteMultiplexerSBV x domain xs
            return f

declareUnnamedMultiplexer :: Bool -> Kind -> [Int] -> (Int -> SBVM SExpr) -> Kind -> SBVM (SExpr -> SExpr)
declareUnnamedMultiplexer isOuter kind domain applydomain kinddomain = do
    sbv_name <- nextName "mux"
    f <- declareMultiplexer isOuter sbv_name kind domain applydomain kinddomain
    return f

declareNamedMultiplexer :: Bool -> Pident -> Kind -> [Int] -> (Int -> SBVM SExpr) -> Kind -> SBVM (SExpr -> SExpr)
declareNamedMultiplexer isOuter name kind domain applydomain kinddomain = do
    let sbv_name = prettyPident name
    f <- declareMultiplexer isOuter sbv_name kind domain applydomain kinddomain
    registerVar name $ Left $ \x -> toSVal $ f (fromSVal x)
    return f

applyUnnamedMultiplexer :: Bool -> Kind -> [Int] -> (Int -> SBVM SExpr) -> Kind -> SExpr -> SBVM SExpr
applyUnnamedMultiplexer isOuter kind domain applydomain kinddomain var = do
    f <- declareUnnamedMultiplexer isOuter kind domain applydomain kinddomain
    return $ f var

applyUnnamedMultiplexers :: Bool -> Kind -> [[Int]] -> ([Int] -> SBVM SExpr) -> [Kind] -> [SExpr] -> SBVM SExpr
applyUnnamedMultiplexers isOuter kind [] applydomains [] [] = applydomains []
applyUnnamedMultiplexers isOuter kind (domain:domains) applydomains (kinddomain:kinddomains) (var:vars) = do
    let rec i = applyUnnamedMultiplexers isOuter kind domains (\is -> applydomains (i:is)) kinddomains vars
    applyUnnamedMultiplexer isOuter kind domain rec kinddomain var

modelToSBVBody :: KLassos -> MultiBmodule -> ((String,Quant),Bool) -> SBVM SExpr
modelToSBVBody kLassos bsmv ((dim,q),isOuter) = do
    ltlToSBool isOuter kLassos (m_ltlspec bsmv)

-- produces a mapping from extended trace (loopNew+lassoNew-1) to original trace (k)
-- extend a trace by producing: 1) a lasso of length lassoNew (repeating the last state); 2) a prefix of length loopNew (repeating the first state)
extendTraceConcrete :: Int -> Int -> Int -> Int -> Int -> Int
extendTraceConcrete loopNew lassoNew l k i' = is' !! i'
    where
    is = [0..k-1]
    (prefix,lasso) = splitAt l is
    prefix' = replicate (loopNew - length prefix) 0 ++ prefix
    lasso' = lasso ++ replicate (lassoNew - length lasso) (k-1)
    is' = prefix' ++ lasso'

renderPident :: Int -> Pident -> SBVM SExpr
renderPident i qn = do
    let qni = addDimPident qn (Peint i)
    liftM unRight $ getVar qni

getVar :: Pident -> SBVM SFunExpr
getVar n = do
    st <- State.get
    case Map.lookup n (sbvVars st) of
        Nothing -> error $ "getVar: " ++ show n ++ show (Map.keys $ sbvVars st)
        Just sv -> return sv

registerVar :: Pident -> SFunExpr -> SBVM ()
registerVar n sv = State.modify $ \st -> st { sbvVars = Map.insert n sv $ sbvVars st }  

registerConst :: Pident -> Bexpr -> SBVM ()
registerConst n e = State.modify $ \st -> st { sbvConsts = Map.insert n e $ sbvConsts st }            

letSBVM :: Pident -> Kind -> SBVM SExpr -> SBVM ()
letSBVM n k m = do
    x <- m
    registerVar n (Right x)
    
modLoop :: Int -> Int -> Int -> Int
modLoop i l k = if i <= l then i else l + ((i-l) `mod` lasso)
    where lasso = k - l

type KnownLoops = Map String Int

type DefinedLTLFun = ([SVal] -> SVal,[Pident])

-- creates SMTLIB define-fun for non-LTL (possibly with X) sub-expressions
-- to avoid code duplication when the unrolling does not depend on loop values
defineLTLFun :: Bexpr -> Kind -> SBVM DefinedLTLFun
defineLTLFun e outkind = do
    funs <- State.gets sbvDefinedLTLFuns
    case HashMap.lookup e funs of
        Just def -> return def
        Nothing -> do
            funname <- nextName "prop"
            st <- getSBVM
            registeredKinds <- State.gets (mapWithKey corePidentDimI (kindOf . unRight) . Map.filter isRight . Map.filterWithKey (\k _ -> k /= loopVar) . sbvVars)
            let ei = replaceNextI 0 e
            let fvars = Set.toList $ bvarSet ei
            let kinds = map (\fvar -> unsafeLookupNote "defineLTLFun" (corePidentDimI fvar) registeredKinds) fvars
            let lookupTable table n = return $ fromSVal $ unsafeLookupNote "defineLTLFun variable not found" n table
            let fun = interpretedFun funname kinds outkind $ \vals -> do
                    let table = Map.fromList $ zip fvars vals
                    res <- liftM toSVal $ evalSBVMWith st $ exprToSBV Nothing (lookupTable table) ei
                    return res
            let def = (fun,fvars)
            State.modify $ \gst -> gst { sbvDefinedLTLFuns = HashMap.insert e def $ sbvDefinedLTLFuns gst }
            return def        

renderCon :: String -> KLassos -> (Int -> Pident -> SBVM SExpr) -> (KnownLoops -> Int -> Pident -> SBVM SExpr)
renderCon dim kLassos render known i n = do
    let loop = unsafeLookupNote ("renderCon loop not found for " ++ show dim) dim known
    render (modLoop i loop $ fst $ unsafeLookup dim kLassos) n

ltlToSBoolConcrete :: Bool -> KLassos -> (Int -> Pident -> SBVM SExpr) -> Bexpr -> SBVM SExpr
ltlToSBoolConcrete areOuter kLassos render ltl = do
    unroll Nothing 0 ltl
  where
    applyLTLFun :: KnownLoops -> Int -> DefinedLTLFun -> SBVM SExpr
    applyLTLFun knowns i (f,fvars) = do
        ivals <- forM fvars $ \fvar -> do
            let Just (n,dim,Just j) = splitPidentDimI fvar
            liftM toSVal $ renderCon dim kLassos render knowns (i+j) (addDimPident n $ mkQuantDim dim)
        return $ fromSVal $ f ivals

    unroll :: Maybe KnownLoops -> Int -> Bexpr -> SBVM SExpr
    unroll (Just knowns) i e | isBoolBexpr e && isFiniteLTLBexpr e && sizeBexpr e > 3 = do
        defineLTLFun e KBool >>= applyLTLFun knowns i
    unroll (Just knowns) i e | isFiniteLTLBexpr e = do
        let ei = replaceNextI i e
        let renderI ni = do
                let Just (n,dim,Just j) = splitPidentDimI ni
                renderCon dim kLassos render knowns j (addDimPident n $ mkQuantDim dim)
        exprToSBV (Just $ Just (i,Just knowns)) renderI ei
    unroll knowns i (Bopn Pand es) = liftM andsSBV $ mapM (unroll knowns i) $ HashSet.toList es
    unroll knowns i (Bopn Por es) = liftM orsSBV $ mapM (unroll knowns i) $ HashSet.toList es
    unroll knowns i (Bop1 Pnot e) = liftM notSBV $ unroll knowns i e
    unroll knowns i (Bop2 Pequiv e1 e2) = do
        b1 <- unroll knowns i e1
        b2 <- unroll knowns i e2
        return $ equivSBV b1 b2
    unroll knowns i (Bop2 Pimplies e1 e2) = do
        b1 <- unroll knowns i e1
        b2 <- unroll knowns i e2
        return $ impliesSBV b1 b2
    unroll knowns i (Bop1 Px e1) = unroll knowns (i+1) e1
    unroll knowns i (Bop2 Pv e1 e2) = liftM notSBV $ unroll knowns i (Bop2 Pu (bnot e1) (bnot e2))
    
    unroll Nothing i ltl = do
        let dims = Set.toList $ bdimSet ltl
        loopVars <- mapM getLoopVar dims
        let ksList = map (\dim -> unsafeLookup dim kLassos) dims
        let unrollLoops loops = do
                let knowns = Map.fromList (zip dims loops)
                unroll (Just knowns) i ltl
        let domains = map (\(k,minLasso) -> [0..k-minLasso]) ksList
        let kinds = map (\(k,minLasso) -> sbvKindWith $ unrollTy $ k-minLasso+1) ksList
        applyUnnamedMultiplexers areOuter KBool domains unrollLoops kinds loopVars            
    
    unroll (Just knowns0) i ltl = do
        let dims = bdimSet ltl
        let knowns = Map.filterWithKey (\d _ -> Set.member d dims) knowns0
        unrollLTL knowns i (i + maxLoopIterations knowns kLassos) ltl
            
    unroll knowns i ltl = error $ "unroll concrete: " ++ show knowns ++ " " ++ show i ++ " " ++ prettyprint ltl
    
    unrollLTL :: KnownLoops -> Int -> Int -> Bexpr -> SBVM SExpr
    unrollLTL knowns jmin jmax ltl@(Bop1 Pg e1) = do
        forallCon [jmin..jmax] $ \j -> do
            unroll (Just knowns) j e1
    unrollLTL knowns jmin jmax ltl@(Bop1 Pf e1) = do
        existsCon [jmin..jmax] $ \j -> do
            unroll (Just knowns) j e1
    unrollLTL knowns jmin jmax ltl@(Bop2 Pu e1 e2) = do
        existsCon [jmin..jmax] $ \j -> do
            b <- unroll (Just knowns) j e2
            liftM (andSBV b) $ do
                forallCon [jmin..j-1] $ \k -> do
                    unroll (Just knowns) k e1
    unrollLTL knowns jmin jmax e = error $ "unrollLTL: " ++ prettyprint e

existsCon :: [Int] -> (Int -> SBVM SExpr) -> SBVM SExpr
existsCon js f = liftM orsSBV $ mapM f js

forallCon :: [Int] -> (Int -> SBVM SExpr) -> SBVM SExpr
forallCon js f = liftM andsSBV $ mapM f js

maxK :: KLassos -> Int
maxK kLassos = product ks
  where
    ks = map (\(k,minLasso) -> k - minLasso + 1) $ Map.elems kLassos

type KLassos = Map String (Int,Int)
type Ks = Map String Int
type PrevKs = Either Ks Ks -- Left = raise outer k; Right = raise inner k

maxLoopIterations :: KnownLoops -> KLassos -> Int
maxLoopIterations knowns ks = loop + lasso - 1
    where
    loops = Map.toList knowns
    loop = maximumNote "maxLoopIterations" $ map snd loops
    lassos = map (\(dim,loop) -> fst (unsafeLookup dim ks) - loop) loops
    lasso = lcms lassos

exprToSBVMulti :: Bool -> Kind -> KLassos -> Kind -> Bexpr -> SBVM SExpr
exprToSBVMulti areOuter kind kLassos kindi e = liftM (castSBV kind) $ do
    let dims = Set.toList $ bdimSet e
    loopVars <- mapM getLoopVar dims
    let ksList = map (\dim -> unsafeLookup dim kLassos) dims
    let unrollLoops loops = do
            let knowns = Map.fromList (zip dims loops)
            let renderI ni = do
                    let Just (n,dim,Just j) = splitPidentDimI ni
                    renderCon dim kLassos renderPident knowns j (addDimPident n $ mkQuantDim dim)
            exprToSBV (Just $ Just (0,Just knowns)) renderI e
    let domains = map (\(k,minLasso) -> [0..k-minLasso]) ksList
    let kinds = map (\(k,minLasso) -> sbvKindWith $ unrollTy $ k - minLasso + 1) ksList
    applyUnnamedMultiplexers areOuter kind domains unrollLoops kinds loopVars    

exprToSBV :: Maybe CacheCtx -> (Pident -> SBVM SExpr) -> Bexpr -> SBVM SExpr
exprToSBV ctx render e = do
    st <- State.get
    case ctx of
        Nothing -> exprToSBV' e
        Just cache -> case getSBVCache cache e st of
            Just sv -> return sv
            Nothing -> do
                sv <- exprToSBV' e
                State.modify $ addSBVCache cache e sv
                return sv
  where
    exprToSBV' :: Bexpr -> SBVM SExpr
    exprToSBV' (Bvar x t) = render x
    exprToSBV' (Bbool b) = return $ boolSBV b
    exprToSBV' (Bint i) = return $ intSBV (sbvKindWith $ VInt $ IntSet.singleton i) i
    exprToSBV' (Bop1 o x) = exprToSBV1 o x
    exprToSBV' (Bop2 o x y) = exprToSBV2 o x y
    exprToSBV' (Bopn Pand xs) = liftM andsSBV $ mapM (exprToSBV ctx render) $ HashSet.toList xs
    exprToSBV' (Bopn Por xs) = liftM orsSBV $ mapM (exprToSBV ctx render) $ HashSet.toList xs
    exprToSBV' e = error $ "exprToSBV' " ++ show e
    
    exprToSBV1 :: Pop1 -> Bexpr -> SBVM SExpr
    exprToSBV1 (Pcast EInt) e1 | isBoolBexpr e1 = do
        e1' <- exprToSBV ctx render e1
        return $ castSBV boolIntKind e1'
    exprToSBV1 Pnot e1 = liftM notSBV $ exprToSBV ctx render e1
    exprToSBV1 o e1 = error $ "exprToSBV1 " ++ show o ++ " " ++ show e1
    
    exprToSBV2 :: Pop2 -> Bexpr -> Bexpr -> SBVM SExpr
    exprToSBV2 Pequiv e1 e2 = do
        b1 <- exprToSBV ctx render e1
        b2 <- exprToSBV ctx render e2
        return $ equivSBV b1 b2
    exprToSBV2 Pimplies e1 e2 = do
        b1 <- exprToSBV ctx render e1
        b2 <- exprToSBV ctx render e2
        return $ impliesSBV b1 b2
    exprToSBV2 o@(isCmpOp2 -> True) e1 e2 = do
        b1 <- exprToSBV ctx render e1
        b2 <- exprToSBV ctx render e2
        return $ compareCastSBV o b1 b2
    exprToSBV2 Pplus e1 e2 = do
        i1 <- exprToSBV ctx render e1
        i2 <- exprToSBV ctx render e2
        return $ plusCastSBV i1 i2
    exprToSBV2 Pminus e1 e2 = do
        i1 <- exprToSBV ctx render e1
        i2 <- exprToSBV ctx render e2
        return $ minusCastSBV i1 i2
    exprToSBV2 Ptimes e1 e2 = do
        i1 <- exprToSBV ctx render e1
        i2 <- exprToSBV ctx render e2
        return $ timesCastSBV i1 i2
    exprToSBV2 Pmod e1 e2 = do
        i1 <- exprToSBV ctx render e1
        i2 <- exprToSBV ctx render e2
        return $ modCastSBV i1 i2
    exprToSBV2 Pin e1 e2 | Prelude.not (isNonDetBexpr e2) = exprToSBV2 Peq e1 e2
    exprToSBV2 Pin e1 (Bints is) = exprToSBV ctx render $ expandBints e1 is
    exprToSBV2 o e1 e2 = error $ "exprToSBV2 " ++ show o ++ " " ++ show e1 ++ " " ++ show e2

refineSMTConfig :: SBVState -> SMTConfig -> SMTConfig
refineSMTConfig st cfg = cfg { isNonModelVar = \s -> isNonModelVar cfg s || nonModelFuns s }
    where
    nonModelFuns :: String -> Bool
    nonModelFuns s = Set.member s (sbvIgnores st)

instance SBVMonadTrans SBVM where
    mapSymbolic f m = SBVM $ Reader.mapReaderT f (unSBVM m)
    liftSymbolic m = SBVM $ lift m

runWithSBVM :: Maybe TimeRef -> Bool -> SMTConfig -> SBVM SExpr -> SBVSt SMTResult
runWithSBVM timeRef isProve cfg prop = do
    st <- State.get
    (res,st') <- liftIO $ runWithSBV timeRef isProve cfg refineSMTConfig sbvTatic (runSBVM prop st)
    rnf res `seq` State.put st'
    return res

satWithSBVM :: Maybe TimeRef -> SMTConfig -> SBVM SExpr -> SBVSt SatResult
satWithSBVM timeRef cfg prop = liftM (SatResult) $ runWithSBVM timeRef False cfg prop

proveWithSBVM :: Maybe TimeRef -> SMTConfig -> SBVM SExpr -> SBVSt ThmResult
proveWithSBVM timeRef cfg prop = liftM (ThmResult) $ runWithSBVM timeRef True cfg prop








