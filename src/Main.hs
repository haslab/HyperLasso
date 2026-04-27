module Main where

import Data.IORef
import Safe
import Control.Monad.IO.Class
import qualified Shelly
import Data.Text (Text(..))
import qualified Data.Text as T
import System.Console.CmdArgs
import System.Environment
import System.Directory
import System.FilePath
import System.IO
import Data.Typeable
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto
import Data.List as List
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Control.Monad.Trans
import Prettyprinter
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Proxy
import qualified Data.Key as K
import Data.Hashable

import IO
import qualified MC
import Utils.Error
import Utils.Misc
import Utils.Pretty as Pretty
import qualified Utils.Location as L
import SMV.Syntax
import SMV.Typing
import SMV.Pretty hiding (SmvMode(..))
import qualified SMV.Pretty as SMV
import SMV.Parser
import SMV.Packed
import SMV.Trace as SMV
import Transform.Bexpr
import Transform.Bpacked
import Transform.Pexpr
import Transform.Substitute
import Transform.Minimize
import Transform.Rename
import Transform.SMV
import Transform.Normalize
import qualified Transform.SMVToSBV as SMT
import qualified MC

data Args = Args
    { input :: [FilePath]
    , formula :: Maybe FilePath
    , k :: [Int]
    , bmc :: Maybe Int
    , smtsolver :: SMT.Solver
    , witness :: Bool
    , minimize :: Bool
    , debug :: Bool
    , complete :: MC.CompleteMC
    , removeTemps :: Bool
    }
    deriving (Data,Typeable,Show,Eq)

defaultArgs :: Args
defaultArgs = Args
    { input = [] &= help "input SMV files" &= name "i"
    , formula = Nothing &= help "input Hyper formula (quantifiers match the order of input SMV files)" &= name "F"
    , k = [] &= help "number of unrolls (one per model)" &= name "k"
    , bmc = Nothing &= help "incremental BMC up to a maximum bound" &= name "b"
    , witness = False &= help "compute witnesses for outermost quantifier block" &= name "w"
    , smtsolver = SMT.defaultSMTSolver &= help "SMT solver to use" &= name "s"
    , minimize = True &= help ("minimize variable names") &= name "m"
    , debug = False &= help "debug mode" &= name "d"
    , complete = MC.Incomplete &= help "turn on complete model checking, by invoking a complete solver after BMC"
    , removeTemps = True &= help ("remove temporary files") &= name "r"
    } &= details ["HyperLasso Bounded Model Checker for SMV"] 
     &= summary "HyperLasso BMC"

data Stats = Stats
    { totalTimeRef :: TimeRef
    , emptyTimeRef :: TimeRef
    , smtTimeRef :: TimeRef
    , mcTimeRef :: TimeRef
    , numIterationsRef :: IORef Int
    }
    
initStats :: IO Stats
initStats = do
    totalTimeRef <- newIORef 0
    emptyTimeRef <- newIORef 0
    smtTimeRef <- newIORef 0
    mcTimeRef <- newIORef 0
    numIterationsRef <- newIORef 0
    return $ Stats totalTimeRef emptyTimeRef smtTimeRef mcTimeRef numIterationsRef

printStats :: Stats -> IO ()
printStats (Stats totalTimeRef emptyTimeRef smtTimeRef mcTimeRef numIterationsRef) = do
    readIORef totalTimeRef >>= \total -> putStrLn $ "Total time: " ++ prettyprint total
    readIORef emptyTimeRef >>= \em -> putStrLn $ "Emptiness Checks time: " ++ prettyprint em
    readIORef smtTimeRef >>= \smt -> putStrLn $ "SMT time: " ++ prettyprint smt
    readIORef mcTimeRef >>= \mc -> putStrLn $ "MC time: " ++ prettyprint mc
    readIORef numIterationsRef >>= \num -> putStrLn $ "Number of iterations: " ++ prettyprint num

incNumIterations :: Stats -> IO ()
incNumIterations stats = atomicModifyIORef' (numIterationsRef stats) $ \i -> (succ i,())

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    when (debug args) $ putStrLn $ "Running with arguments " ++ show args
    mainSMT args

mainSMT :: Args -> IO ()
mainSMT args = do
    stats <- initStats
    let ks = consMaybe (bmc args) (k args)
    when (length ks <= 0) $ exitWithErrorMessage "Please provide at least one unroll bound k or set bmc"
    when (any (< 0) ks) $ exitWithErrorMessage "Please specify >= 0 unroll bound"
    when (isNothing (formula args)) $ exitWithErrorMessage "Please specify input formula file"
    measureTimeRef' (totalTimeRef stats) $ doInputs args (input args) $ \infiles1 inps -> doSMT stats args infiles1 inps
    printStats stats

doInputs :: Args -> [FilePath] -> ([FilePath] -> ([PackedPmodule],[(Digest SHA256,PackedBmodule)],Bformula,[Subst]) -> IO a) -> IO a
doInputs args infiles go = do
    
    when (debug args) $ putStrLn "Parsing input SMVs and hyper formula"
    (insmvs,qvars,formula) <- do
        insmvs <- readSMVs infiles
        let tys = map (moduleTypes . snd) insmvs
        let sss = map (moduleSubst . snd) insmvs
        formula <- liftM (runIdentity . substFormula sss True) $ readFormula (debug args) (fromJustNote "doInputs" $ formula args) tys
        let qs = quantsPformula formula
        let qvars = map snd $ groupVarSet (map fst qs) $ varsFormula formula
        return (insmvs,qvars,formula)
    
    when (debug args) $ putStrLn "Processing input SMVs and hyper formula"
    (midsmvs,names) <- liftM (unzip . map assocl) $ do
        mapDigestM (doSMV args) $ map assocr $ zip insmvs qvars
    
    (outsmvs,outformula) <- do
        let qs = quantsPformula formula
        let vars = map (b_vars . snd) midsmvs
        let hnames = joinHyperNameSubst $ zip (map fst qs) names
        let nformula = renameFormula hnames formula
        bformula <- doBM (Map.map toVarType $ joinHyperPvars $ zip (map fst qs) vars) $ toBformula nformula
        return (midsmvs,bformula)

    let inps = (map snd insmvs,outsmvs,outformula,map fromNameSubst names)
    go infiles inps

doSMV :: Args -> (PackedPmodule,Set Pident) -> IO (PackedBmodule,NameSubst)
doSMV args (smv,used) = doBMState $ do
    smv1 <- toPackedBmodule smv
    let smv2 = dropUnusedBmoduleVars used smv1
    if minimize args
        then transformBminimize smv2
        else return (smv2,idNameSubst $ b_vars smv2)

doSMT :: (Eq digest) => Stats -> Args -> [FilePath] -> ([PackedPmodule],[(digest,PackedBmodule)],Bformula,[Subst]) -> IO ()
doSMT stats args infiles (insmvs,smvs,formula,names) = do
    let smtcfg = SMT.smtConfig (debug args) (smtsolver args)
    let qs = quantsBformula formula
    let dims = map fst qs
    let ks0 = case bmc args of
            Nothing -> map succ $ k args
            Just _ -> [1]
    let ks = Map.fromList $ zip dims (repeatLast ks0)
    let prevks = Left $ Map.fromList $ zip dims $ repeat 0
    let multis = map (\(dim,(dig,smv)) -> (dig,toMultiBmodule dim smv)) $ zip dims smvs
    ((ty,traces),isComplete) <- SMT.evalSBVSt_ $ MC.runMCM (map snd multis) formula ks prevks $ do
        MC.normalizeMC (emptyTimeRef stats) (removeTemps args) (debug args) -- only at the beginning
        oriFormula <- State.gets MC.mcFormula
        iterateSMT stats args smtcfg names oriFormula (fmap (const 1) $ bmc args)
    when (witness args) $ writeTraces (debug args) qs traces
    let completeness = if isComplete then "Complete" else "Incomplete"
    putStrLn $ show $ pretty ty <+> Pretty.parens completeness

-- | Iterate for increasing ks
iterateSMT :: Stats -> Args -> SMT.SMTConfig -> [Subst] -> Bformula -> Maybe Int -> MC.MCM SMT.SBVSt MC.CompleteSBVResult
iterateSMT stats args smtcfg names oriFormula doBMC = do
    ks <- State.gets MC.mcKs
    prevks <- State.gets MC.mcPrevKs
    when (debug args) $ liftIO $ putStrLn $ "Running BMC with k bounds " ++ prettyprint ks ++ " after " ++ prettyprint prevks
    (ty,traces,sourceTraces) <- stepSMT stats args smtcfg names
    when (debug args) $ liftIO $ forM_ traces $ \mb -> forM_ mb $ \tr -> putStrLn $ "Found candidate trace " ++ prettyprint tr
    mb <- MC.completeMC (Just $ mcTimeRef stats) (removeTemps args) (debug args) (complete args) (ty,traces)
    case mb of
        MC.MCUnknown -> if stopBMC 
            then do -- reached BMC limit, return inconclusive result
                liftIO $ putStrLn $ "stopping BMC " ++ show doBMC
                return ((ty,sourceTraces),False)
            else do -- did not find counter-example, step outer ks
                liftIO $ putStrLn $ "stepping BMC " ++ show doBMC
                let nextBMC = fmap succ doBMC 
                let outerk = fromJust nextBMC
                let innerk = outerk
                MC.resetFormulaMC oriFormula outerk innerk
                liftIO $ incNumIterations stats
                iterateSMT stats args smtcfg names oriFormula nextBMC
        MC.MCUnsat -> do -- definitive result
            return ((ty,sourceTraces),True)
        MC.MCSat counterk -> do -- found counter-example, step internal ks
            let innerk :: Int -> Int = (const counterk) 
            MC.negateMCFormula traces innerk
            liftIO $ incNumIterations stats
            iterateSMT stats args smtcfg names oriFormula doBMC
  where
    stopBMC = case doBMC of
        Nothing -> True
        Just curk -> case bmc args of
            Nothing -> True
            Just maxk -> curk >= maxk

-- | Run BMC for specific ks
stepSMT :: Stats -> Args -> SMT.SMTConfig -> [Subst] -> MC.MCM SMT.SBVSt (SMT.ResultType,[Maybe Trace],[Maybe Trace])
stepSMT stats args smtcfg names = do
    lift SMT.resetSBV
    st <- State.get
    let multis = MC.mcMultis st
    let formula = MC.mcFormula st
    let ks = MC.mcKs st
    let prevks = MC.mcPrevKs st
    let isWitness = witness args || (complete args /= MC.Incomplete)
    (ty,traces) <- lift $ SMT.transformBsToSBV (Just $ smtTimeRef stats) smtcfg ks prevks multis formula isWitness
    let mkSourceTrace (name,mbtr) = case mbtr of
            Nothing -> (Nothing,Nothing)
            Just tr -> case checkStaticTrace True (composeSubstTrace name tr) of
                Nothing -> (Nothing,Nothing)
                Just sourceTr -> (Just tr,Just sourceTr)
    let (unzip -> (traces',sourceTraces')) = map mkSourceTrace (zip names traces)
    when (debug args) $ liftIO $ putStrLn $ "BMC result " ++ prettyprint ty
    return (ty,traces',sourceTraces')

