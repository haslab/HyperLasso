{-# LANGUAGE GADTs, BangPatterns, StandaloneDeriving #-}

module SMT.SBV 
    ( module SMT.SBV 
    , SBV(..),Symbolic(..),Query(..),SBool,SInteger,SMTConfig(..),SatResult(..),ThmResult(..),SMTResult(..),Kind(..),HasKind(..)
    , SVal,Solver(..),CV(..)
    , SMTModel(..), uninterpretedFun, interpretedFun
    ) where

import Control.Concurrent.MVar as MVar
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.Data
import GHC.Generics
import Control.Exception
import Control.DeepSeq
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IntSet
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.SBV (SBV(..),Symbolic(..),MonadSymbolic(..),SBool,SInteger,SMTConfig(..),SatResult(..),ThmResult(..),SMTResult(..),Kind(..),HasKind(..))
import qualified Data.SBV as SBV
import Data.SBV.Internals (uninterpretedFun, interpretedFun, SMTModel(..))
import qualified Data.SBV.Internals as SBV hiding (free)
import Data.SBV.Control (Query(..))
import qualified Data.SBV.Control as SBV
import qualified Data.SBV.Tools.Overflow as SBV
import Data.SBV.Dynamic (SVal(..),Solver(..),CV(..))
import qualified Data.SBV.Dynamic as DSBV
import Data.Maybe

import SMV.Syntax
import SMV.Typing
import Transform.Pexpr
import Utils.Misc

unSBV :: SBV a -> SVal
unSBV (SBV.SBV a) = a

enumerateKind :: SBV.Kind -> Maybe IntSet
enumerateKind (SBV.KBounded False n) = Just $ IntSet.fromList [0..2^n-1]
enumerateKind (SBV.KBounded True n) = Just $ IntSet.fromList [-2^(n-1)..2^(n-1)-1]
enumerateKind k = Nothing

sbvKind :: VarType -> SBV.Kind
sbvKind VBool = SBV.KBool
sbvKind (VInt is) = if IntSet.findMin is < 0 then mkInt is else mkWord is

-- bitvectors need at least 1 bit
mkWord :: IntSet -> SBV.Kind
mkWord is = SBV.KBounded False $ max 1 $ nextExponentOf2 (IntSet.findMax is + 1)

-- bitvectors need at least 1 bit
mkInt :: IntSet -> SBV.Kind
mkInt is = SBV.KBounded True $ max 1 ((max maxExp minExp) + 1)
    where
    maxExp = nextExponentOf2 (IntSet.findMax is + 1)
    minExp = nextExponentOf2 (-(IntSet.findMin is))

fromSV :: SBV.SV -> SBV.Kind -> SBV.SVal
fromSV sv k = SBV.SVal k (Right $ SBV.cache $ const $ return sv)

-- NOTE: this is dangerous, as the inner computation is performed lazily.
-- Particular care when wrapping inside SBVM, may interfere with the monad side effects
quantifiedSBool :: Symbolic SBool -> Symbolic SBool
quantifiedSBool m = return $ SBV.quantifiedBool m

quantifiedSExpr :: Symbolic SExpr -> Symbolic SExpr
quantifiedSExpr m = liftM fromSBool $ quantifiedSBool (liftM toSBool m)

quantToSBV :: Quant -> DSBV.Quantifier
quantToSBV Qforall = SBV.ALL
quantToSBV Qexists = SBV.EX

quantifiedVar :: SBVMonad m => Quant -> SBV.Kind -> m SBV.SVal
quantifiedVar q ky = do
    env <- SBV.symbolicEnv
    y <- liftIO $ SBV.quantVar (quantToSBV q) env ky
    return $ fromSV y ky

instance (SBV.Constraint Symbolic a) => SBV.Constraint Symbolic (Symbolic a) where
    mkConstraint st ma = ma >>= \a -> SBV.mkConstraint st a

instance (SBV.Constraint Query a) => SBV.Constraint Query (Query a) where
    mkConstraint st ma = ma >>= \a -> SBV.mkConstraint st a

modifyQueryCfg :: (SMTConfig -> SMTConfig) -> Query ()
modifyQueryCfg f = do
    SBV.modifyQueryState $ \qst -> qst { SBV.queryConfig = f $ SBV.queryConfig qst }

externalQuery :: Query a -> Symbolic a
externalQuery = SBV.query

internalQuery :: Query a -> Symbolic a
internalQuery = SBV.executeQuery SBV.QueryInternal

getSMTResultWith :: (SMTConfig -> SMTConfig) -> Maybe String -> Query SMTResult
getSMTResultWith fcfg tatic = do
    modifyQueryCfg fcfg
    case tatic of
        Nothing -> SBV.getSMTResult
        Just using -> SBV.getSMTResultUsing using

runWithSBV :: Maybe TimeRef -> Bool -> SMTConfig -> (st -> SMTConfig -> SMTConfig) -> Maybe String -> Symbolic (SExpr,st) -> IO (SMTResult,st)
runWithSBV timeRef isProve cfg fcfg tatic a = do
    ref <- newEmptyMVar
    let reduce m = do
            (b,st) <- m
            liftIO $ putMVar ref st
            return (toSBool b)
    let getRes = do
            st <- liftIO $ readMVar ref
            SBV.modifyQueryState $ \qst -> qst { SBV.queryConfig = fcfg st $ SBV.queryConfig qst }
            maybeMeasureTimeRef' timeRef $ case tatic of
                Nothing -> SBV.getSMTResult
                Just using -> SBV.getSMTResultUsing using
    r <- SBV.runWithQuery reduce (not isProve) getRes cfg a
    st <- readMVar ref
    return (r,st)

proveWithSBV :: Maybe TimeRef -> SMTConfig -> (st -> SMTConfig -> SMTConfig) -> Maybe String -> Symbolic (SExpr,st) -> IO (ThmResult,st)
proveWithSBV timeRef cfg fcfg tatic a = liftM (ThmResult >< id) $ runWithSBV timeRef True cfg fcfg tatic a

satWithSBV :: Maybe TimeRef -> SMTConfig -> (st -> SMTConfig -> SMTConfig) -> Maybe String -> Symbolic (SExpr,st) -> IO (SatResult,st)
satWithSBV timeRef cfg fcfg tatic a = liftM (SatResult >< id) $ runWithSBV timeRef False cfg fcfg tatic a      

mkIntRangeSBool :: SExpr -> Maybe IntSet -> Maybe IntSet -> SExpr
mkIntRangeSBool v mbis mbts = go mbis mbts
  where
    go (Just is) (Just ts)
        | IntSet.size is == 0 = falseSBV
        | is == ts = trueSBV
        | IntSet.size (IntSet.difference ts is) < IntSet.size is = notSBV $ mk (IntSet.toList $ IntSet.difference ts is)
        | otherwise = mk (IntSet.toList is)
    go (Just is) Nothing = mk (IntSet.toList is)
    go Nothing _ = trueSBV
    
    mint = fmap IntSet.findMin mbts
    maxt = fmap IntSet.findMax mbts
    kind = SBV.kindOf v
    vop op i = compareSBV op v (intSBV kind i)
    mk :: [Int] -> SExpr
    mk [i] = vop Peq i
    mk [i,j] = vop Peq i `orSBV` vop Peq j
    mk is = case isRange is of
        Just (i,j) ->
            (if mint==Just i then trueSBV else vop Pgeq i)
            `andSBV`
            (if maxt==Just j then trueSBV else vop Pleq j)
        Nothing -> orsSBV $ map (\i -> vop Peq i) is

-- SBV caching is aggressive, which leads to sub-optimal constant foldings
-- we perform operations on this intermediate type to delay sending computating to SBV, and generate smaller expressions
data SExpr
    = SConst SBV.CV
    | SVal SBV.SVal
    | SOp1 Pop1 SExpr
    | SOp2 Pop2 SExpr SExpr
    | SOpn Popn [SExpr]
  deriving (Show,Generic)

instance SBVNFData SExpr where
    forceSBV (SConst cv) = do
        cv' <- forceSBV cv
        return $! SConst cv'
    forceSBV (SVal sv) = do
        sv' <- forceSBV sv
        return $! SVal sv'
    forceSBV (SOp1 o e1) = do
        e1' <- forceSBV e1
        return $! SOp1 o e1'
    forceSBV (SOp2 o e1 e2) = do
        e1' <- forceSBV e1
        e2' <- forceSBV e2
        return $! SOp2 o e1' e2'
    forceSBV (SOpn o es) = do
        es' <- mapM forceSBV es
        return $! SOpn o es'

instance SBV.HasKind SExpr where
    kindOf (SConst cv) = SBV.kindOf cv
    kindOf (SVal sv) = SBV.kindOf sv
    kindOf (SOp1 Pnot e) = SBV.KBool
    kindOf (SOp2 (isCmpOp2 -> True) e1 e2) = SBV.KBool
--    kindOf (SOp2 Pplus x y) = sbvPlusKind (SBV.kindOf x) (SBV.kindOf y)
--    kindOf (SOp2 Pminus x y) = sbvMinusKind (SBV.kindOf x) (SBV.kindOf y)
--    kindOf (SOp2 Ptimes x y) = sbvTimesKind (SBV.kindOf x) (SBV.kindOf y)
    kindOf (SOpn Pand es) = SBV.KBool
    kindOf (SOpn Por es) = SBV.KBool

compareSBV :: Pop2 -> SExpr -> SExpr -> SExpr
compareSBV op e1 e2 = SOp2 op e1 e2

compareCastSBV :: Pop2 -> SExpr -> SExpr -> SExpr
compareCastSBV op e1 e2 = compareSBV op v1 v2
    where (v1,v2) = unifySBV e1 e2

svCompare :: Pop2 -> SVal -> SVal -> SVal
svCompare Peq = DSBV.svEqual
svCompare Pneq = DSBV.svNotEqual
svCompare Plt = DSBV.svLessThan
svCompare Pleq = DSBV.svLessEq
svCompare Pgt = DSBV.svGreaterThan
svCompare Pgeq = DSBV.svGreaterEq

andSBV :: SExpr -> SExpr -> SExpr
andSBV (SOpn Pand xs) (SOpn Pand ys) = SOpn Pand (xs ++ ys)
andSBV (SOpn Pand xs) y = SOpn Pand (y : xs)
andSBV x (SOpn Pand ys) = SOpn Pand (x : ys)
andSBV x y = SOpn Pand [x,y]

andsSBV :: Foldable t => t SExpr -> SExpr
andsSBV = foldl andSBV trueSBV

orSBV :: SExpr -> SExpr -> SExpr
orSBV (SOpn Por xs) (SOpn Por ys) = SOpn Por (xs ++ ys)
orSBV (SOpn Por xs) y = SOpn Por (y : xs)
orSBV x (SOpn Por ys) = SOpn Por (x : ys)
orSBV x y = SOpn Por [x,y]

orsSBV :: Foldable t => t SExpr -> SExpr
orsSBV = foldl orSBV falseSBV

notSBV :: SExpr -> SExpr
notSBV (SOp1 Pnot e) = e
notSBV (SOp2 o@(isCmpOp2 -> True) x y) = SOp2 (negCmpOp2 o) x y
--notSBV (SOp2 Pequiv x y) = SOp2 Pneq x y
--notSBV (SOp2 Pimplies x y) = notSBV $ (notSBV x) `orSBV` y
notSBV (SOpn Pand es) = SOpn Por (map notSBV es)
notSBV (SOpn Por es) = SOpn Pand (map notSBV es)
notSBV x = liftSBV DSBV.svNot x

minSBV :: SExpr -> SExpr -> SExpr
minSBV x y = iteSBV (compareSBV Pleq x y) x y

minimumSBV :: [SExpr] -> Maybe SExpr
minimumSBV [] = Nothing
minimumSBV xs = Just $ foldl1 minSBV xs

maxSBV :: SExpr -> SExpr -> SExpr
maxSBV x y = iteSBV (compareSBV Pleq x y) y x

maximumSBV :: [SExpr] -> Maybe SExpr
maximumSBV [] = Nothing
maximumSBV xs = Just $ foldl1 maxSBV xs

productSBV :: [SExpr] -> Maybe SExpr
productSBV [] = Nothing
productSBV xs = Just $ foldl1 timesSBV xs

summationSBV :: [SExpr] -> Maybe SExpr
summationSBV [] = Nothing
summationSBV xs = Just $ foldl1 plusSBV xs

unifySBV :: SExpr -> SExpr -> (SExpr,SExpr)
unifySBV v1 v2 = (castSBV k12 v1,castSBV k12 v2)
    where k12 = sbvUnifyKind (SBV.kindOf v1) (SBV.kindOf v2)

sbvUnifyKind :: SBV.Kind -> SBV.Kind -> SBV.Kind
sbvUnifyKind k1 k2 | k1 == k2 = k1
sbvUnifyKind (SBV.KBounded sign1 n1) (SBV.KBounded sign2 n2) = SBV.KBounded (max sign1 sign2) (max n1 n2)
sbvUnifyKind SBV.KUnbounded k2 = SBV.KUnbounded
sbvUnifyKind k1 SBV.KUnbounded = SBV.KUnbounded
sbvUnifyKind KBool k2 = sbvUnifyKind boolIntKind k2
sbvUnifyKind k1 KBool = sbvUnifyKind k1 boolIntKind
sbvUnifyKind k1 k2 = error $ "sbvUnifyKind: " ++ show k1 ++ " " ++ show k2

boolIntKind :: Kind
boolIntKind = (SBV.KBounded False 1)

plusCastSBV :: SExpr -> SExpr -> SExpr
plusCastSBV v1 v2 = plusSBV (castSBV k12 v1) (castSBV k12 v2)
    where k12 = sbvPlusKind (SBV.kindOf v1) (SBV.kindOf v2)

plusSBV :: SExpr -> SExpr -> SExpr
plusSBV = lift2SBV DSBV.svPlus

sbvPlusKind :: SBV.Kind -> SBV.Kind -> SBV.Kind
sbvPlusKind k1 k2 | k1 == k2 = k1
sbvPlusKind (SBV.KBounded sign1 n1) (SBV.KBounded sign2 n2) | sign1 == sign2 = SBV.KBounded (max sign1 sign2) (max n1 n2 + 1)
sbvPlusKind SBV.KUnbounded k2 = SBV.KUnbounded
sbvPlusKind k1 SBV.KUnbounded = SBV.KUnbounded
sbvPlusKind KBool k2 = sbvPlusKind boolIntKind k2
sbvPlusKind k1 KBool = sbvPlusKind k1 boolIntKind
sbvPlusKind k1 k2 = error $ "sbvPlusKind: " ++ show k1 ++ " " ++ show k2

minusCastSBV :: SExpr -> SExpr -> SExpr
minusCastSBV v1 v2 = minusSBV (castSBV k12 v1) (castSBV k12 v2)
    where k12 = sbvMinusKind (SBV.kindOf v1) (SBV.kindOf v2)
    
minusSBV :: SExpr -> SExpr -> SExpr
minusSBV = lift2SBV DSBV.svMinus
    
sbvMinusKind :: SBV.Kind -> SBV.Kind -> SBV.Kind
sbvMinusKind k1 k2 | k1 == k2 = k1
sbvMinusKind (SBV.KBounded _ n1) (SBV.KBounded _ n2) = SBV.KBounded True (max n1 n2 + 1)
sbvMinusKind SBV.KUnbounded k2 = SBV.KUnbounded
sbvMinusKind k1 SBV.KUnbounded = SBV.KUnbounded
sbvMinusKind KBool k2 = sbvMinusKind boolIntKind k2
sbvMinusKind k1 KBool = sbvMinusKind k1 boolIntKind
sbvMinusKind k1 k2 = error $ "sbvMinusKind: " ++ show k1 ++ " " ++ show k2

timesCastSBV :: SExpr -> SExpr -> SExpr
timesCastSBV v1 v2 = timesSBV (castSBV k12 v1) (castSBV k12 v2)
    where k12 = sbvTimesKind (SBV.kindOf v1) (SBV.kindOf v2)

timesSBV :: SExpr -> SExpr -> SExpr
timesSBV = lift2SBV DSBV.svTimes
    
sbvTimesKind :: SBV.Kind -> SBV.Kind -> SBV.Kind
sbvTimesKind k1 k2 | k1 == k2 = k1
sbvTimesKind (SBV.KBounded sign1 n1) (SBV.KBounded sign2 n2) | sign1 == sign2 = SBV.KBounded (max sign1 sign2) (n1 + n2)
sbvTimesKind SBV.KUnbounded k2 = SBV.KUnbounded
sbvTimesKind k1 SBV.KUnbounded = SBV.KUnbounded
sbvTimesKind KBool k2 = sbvTimesKind boolIntKind k2
sbvTimesKind k1 KBool = sbvTimesKind k1 boolIntKind
sbvTimesKind k1 k2 = error $ "sbvTimesKind: " ++ show k1 ++ " " ++ show k2

modCastSBV :: SExpr -> SExpr -> SExpr
modCastSBV v1 v2 = modSBV (castSBV k12 v1) (castSBV k12 v2)
    where k12 = sbvModKind (SBV.kindOf v1) (SBV.kindOf v2)

modSBV = remSBV

sbvModKind :: SBV.Kind -> SBV.Kind -> SBV.Kind
sbvModKind k1 k2 | k1 == k2 = k1
sbvModKind (SBV.KBounded False n1) (SBV.KBounded False n2) = SBV.KBounded False (n1 + n2)
sbvModKind k1 k2 = error $ "sbvModKind unsupported " ++ show k1 ++" "++ show k2 

trueSBV :: SExpr
trueSBV = SConst SBV.trueCV

falseSBV :: SExpr
falseSBV = SConst SBV.falseCV

equivSBV :: SExpr -> SExpr -> SExpr
equivSBV x y = SOp2 Peq x y

impliesSBV :: SExpr -> SExpr -> SExpr
--impliesSBV = lift2SBV DSBV.svImplies
impliesSBV x y = (notSBV x) `orSBV` y

isFalseSBV :: SExpr -> Bool
isFalseSBV (SConst cv) = cv==SBV.falseCV
isFalseSBV e = False

isTrueSBV :: SExpr -> Bool
isTrueSBV (SConst cv) = cv==SBV.trueCV
isTrueSBV e = False

boolSBV :: Bool -> SExpr
boolSBV False = falseSBV
boolSBV True = trueSBV

intSBV :: SBV.Kind -> Int -> SExpr
intSBV k i = fromSVal $ intSVal k i

intSVal :: SBV.Kind -> Int -> SVal
intSVal k i = DSBV.svInteger k $ toEnum i

decrementSBV :: SExpr -> SExpr
decrementSBV = liftSBV DSBV.svDecrement

incrementSBV :: SExpr -> SExpr
incrementSBV = liftSBV DSBV.svIncrement

incSBV :: Int -> SExpr -> SExpr
incSBV i v = plusSBV v $ intSBV (kindOf v) i

castSBV :: SBV.Kind -> SExpr -> SExpr
castSBV k e = case (ke,k) of
    -- (from,to)
    (ke,(==ke) -> True) -> e
    (KBool,KBounded _ _) -> fromSVal $ DSBV.svIte (toSVal e) (intSVal k 1) (intSVal k 0)
    (KBounded _ _,KBounded _ _) -> liftSBV (DSBV.svFromIntegral k) e
    (KBounded _ _,KBool) -> fromSVal $ DSBV.svIte (toSVal $ compareSBV Pneq e $ intSBV ke 0) (toSVal trueSBV) (toSVal falseSBV)
    otherwise -> error $ "castSBV unsupported from " ++ show (kindOf e) ++" to "++ show k
  where
    ke = kindOf e

svalToCV :: SVal -> Maybe CV
svalToCV (SBV.SVal k (Left cv)) = Just cv
svalToCV _ = Nothing

unIntSBV :: SExpr -> Maybe Int
unIntSBV (SConst cv) = let (i :: Integer) = SBV.fromCV cv in Just (fromEnum i)
unIntSBV (SVal (SBV.SVal k (Left cv))) = unIntSBV (SConst cv)
unIntSBV _ = Nothing

iteSBV :: SExpr -> SExpr -> SExpr -> SExpr
iteSBV = lift3SBV DSBV.svIte

fromSBool :: SBool -> SExpr
fromSBool b = fromSVal $ unSBV b

toSBool :: SExpr -> SBool
toSBool e = sValToSBool $ toSVal e

toSVal :: SExpr -> SVal
toSVal (SConst cv) = SBV.SVal (SBV.kindOf cv) (Left cv)
toSVal (SVal sv) = sv
toSVal (SOp1 Pnot e) = DSBV.svNot (toSVal e)
toSVal (SOp2 o@(isCmpOp2 -> True) x y) = svCompare o (toSVal x) (toSVal y)
--toSVal (SOp2 Pequiv x y) = DSBV.svEqual (toSVal x) (toSVal y)
toSVal (SOpn Pand es) = foldl DSBV.svAnd DSBV.svTrue (map toSVal es)
toSVal (SOpn Por es) = foldl DSBV.svOr DSBV.svFalse (map toSVal es)

fromSVal :: SVal -> SExpr
fromSVal (svalToCV -> Just cv) = SConst cv
fromSVal v = SVal v

liftSBV :: (SVal -> SVal) -> SExpr -> SExpr
liftSBV f e = fromSVal $ f (toSVal e)

lift2SBV :: (SVal -> SVal -> SVal) -> SExpr -> SExpr -> SExpr
lift2SBV f x y = fromSVal $ f (toSVal x) (toSVal y)

lift3SBV :: (SVal -> SVal -> SVal -> SVal) -> SExpr -> SExpr -> SExpr -> SExpr
lift3SBV f x y z = fromSVal $ f (toSVal x) (toSVal y) (toSVal z)

remSBV :: SExpr -> SExpr -> SExpr
remSBV = lift2SBV DSBV.svRem

bvSubOSBV :: SExpr -> SExpr -> SExpr
bvSubOSBV x y = fromSBool $ SBV.bvSubO (toSVal x) (toSVal y)

sValToSBool :: SVal -> SBool
sValToSBool sv = case SBV.kindOf sv of
    SBV.KBool -> SBV.SBV sv
    k -> error $ "sValToSBool: expected a bool but found a " ++ show k

-- assumes a sorted domain
iteMultiplexerSBV :: SExpr -> [Int] -> [SExpr] -> SExpr
iteMultiplexerSBV index domain formulas = walk (zip domain formulas)
  where
    walk []  = error "empty mux"
    walk [(_, f)] = f
    walk xs =
        let (left, right) = splitAt (length xs `div` 2) xs
            pivot = fst (head right)
            condition = compareSBV Plt index $ intSBV (SBV.kindOf index) pivot
        in iteSBV condition (walk left) (walk right)

unSatResult :: SatResult -> (SMTResult,Bool)
unSatResult (SatResult r) = (r,False)

unThmResult :: ThmResult -> (SMTResult,Bool)
unThmResult (ThmResult r) = (r,True)

cvToExpr :: SBV.CV -> Pexpr
cvToExpr cv@(SBV.CV SBV.KBool v) = Pebool $ SBV.cvToBool cv
cvToExpr (SBV.CV _ (SBV.CInteger i)) = Peint $ fromEnum i
cvToExpr cv = error $ "cvToExpr: " ++ show cv

intCV :: Kind -> Int -> SBV.CV
intCV k i = SBV.CV k $ SBV.CInteger $ toEnum i

class SBVNFData a where
    forceSBV :: a -> Symbolic a

instance SBVNFData DSBV.CV where
    forceSBV v = evaluateIO v

instance SBVNFData SVal where
    forceSBV v = do
        sv <- SBV.svToSymSV v
        !() <- liftIO $! SBV.forceSVArg sv
        let v' = fromSV sv (SBV.kindOf v)
        evaluateIO v'
  
instance SBVNFData SBool where
    forceSBV (SBV.SBV v) = liftM SBV.SBV $ forceSBV v
    
instance {-# OVERLAPPABLE #-} NFData a => SBVNFData a where
    forceSBV = return . force
        
evaluateIO :: (MonadIO m,NFData a) => a -> m a
evaluateIO a = do
    !() <- liftIO $! evaluate $! rnf a
    return a

smtCfg :: Solver -> SMTConfig
smtCfg Z3 = SBV.z3 { solverSetOptions =
    [ SBV.OptionKeyword ":smt.macro_finder" ["true"]
    , SBV.OptionKeyword ":smt.mbqi.max_iterations" ["10000"]
    , SBV.OptionKeyword ":smt.auto_config" ["false"]
    , SBV.OptionKeyword ":smt.pull_nested_quantifiers" ["true"]
    , SBV.OptionKeyword ":parallel.enable" ["true"]
    , SBV.OptionKeyword ":parallel.threads.max" ["4"]
    ] }

sbvTatic :: Maybe String
sbvTatic = Just "(check-sat-using (then simplify propagate-values solve-eqs ackermannize_bv qe-light smt))"

defaultSMTSolver :: Solver
defaultSMTSolver = Z3

type SFun = SVal -> SVal

selectSBV :: Either SFun SExpr -> SExpr -> SExpr
selectSBV (Left f) x = liftSBV f x
selectSBV (Right arr) x = lift2SBV DSBV.svReadArray arr x

smtDebug :: Bool -> SMTConfig -> SMTConfig
smtDebug isDebug cfg = if isDebug
    then cfg { SBV.transcript = Just "transcript.smt" } 
    else cfg

constrainSBool_ :: SBVMonadTrans m => SBool -> m ()
constrainSBool_ c = liftSymbolic $ SBV.constrain c

constrainSBV_ :: SBVMonadTrans m => SExpr -> m ()
constrainSBV_ c = constrainSBool_ (toSBool c)

constrainSBV :: SBVMonadTrans m => (Quant,Bool) -> SExpr -> m SExpr -> m SExpr
constrainSBV (q,True) c m = do
    liftSymbolic $ SBV.constrain (toSBool c)
    m
constrainSBV (Qforall,False) c m = liftM (impliesSBV c) m
constrainSBV (Qexists,False) c m = liftM (andSBV c) m

constrainSBVs :: SBVMonadTrans m => (Quant,Bool) -> [SExpr] -> m SExpr -> m SExpr
constrainSBVs q [] m = m
constrainSBVs q (c:cs) m = constrainSBV q c $ constrainSBVs q cs m

class (MonadIO m,MonadSymbolic m) => SBVMonad m where
    freeVar :: String -> SBV.Kind -> m SVal

instance SBVMonad Symbolic where
    freeVar n t = DSBV.svNewVar t n

instance SBVMonad Query where
    freeVar n t = DSBV.svFreshVar t n

outputSBV :: SExpr -> Symbolic ()
outputSBV e = DSBV.outputSVal (toSVal e)

class MonadIO m => SBVMonadTrans m where
    liftSymbolic :: Symbolic a -> m a
    mapSymbolic :: (Symbolic a -> Symbolic b) -> m a -> m b

deriving instance Eq Solver
deriving instance Data Solver

bitpackSBV :: Kind -> Int -> (Kind,[SExpr -> SExpr])
bitpackSBV k@(KBool) num = (k',projs)
    where
    k' = KBounded False num
    projs = map (\i -> castSBV k . liftSBV (DSBV.svExtract i i)) [0..num-1]
bitpackSBV k@(KBounded sign sz) num = (k',projs)
    where
    k' = KBounded sign (sz * num)
    projs = map (\i -> castSBV k . liftSBV (DSBV.svExtract (i*sz+sz-1) (i*sz))) [0..num-1]
bitpackSBV k num = error $ "bitpackSBV unsupported " ++ show k ++" "++ show num

constToSBV :: Kind -> Pexpr -> SExpr
constToSBV k (Peint i) = intSBV k i
constToSBV k (Pebool b) = boolSBV b 



