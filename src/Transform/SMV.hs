module Transform.SMV where

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IntSet
import qualified Data.HashSet as HashSet
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Key as K
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Data.List as List
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BS
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto
import Control.Monad.IO.Class
import Control.Monad
import Prettyprinter

import SMV.Syntax
import SMV.Pretty
import SMV.Parser
import SMV.Packed
import SMV.Typing
import qualified Utils.Location as L
import Utils.Misc
import Utils.Error

transformDeclarative :: Monad m => PackedPmodule -> m PackedPmodule
transformDeclarative pmodule@(PackedPmodule name vars defines init invar trans assigns ltlspec) = do
    let (initAss,transAss) = assignsToExprs assigns
    let init' = pands [init,initAss]
    let trans' = pands [trans,transAss]
    let vars' = Map.map simplifyType vars
    return (PackedPmodule name vars' defines init' invar trans' noPackedPassigns ltlspec)

transformNoInvar :: Monad m => PackedPmodule -> m PackedPmodule
transformNoInvar p = transformDeclarative p >>= \pmodule -> do
    let definedVars = Map.keysSet (p_vars pmodule)
    let init' = pands [p_invar pmodule,p_init pmodule]
    let trans' = pands [p_invar pmodule,Peop1 Pnext (p_invar pmodule),p_trans pmodule]
    return $ pmodule { p_init = init', p_trans = trans' }

assignsToExprs :: PackedPassigns -> (Pexpr,Pexpr)
assignsToExprs (PackedPassigns i n) = (initsToExpr i,nextsToExpr n)

initsToExpr :: PackedPdefs -> Pexpr
initsToExpr = pands . Map.foldrWithKey go []
    where go n e acc = (Peop2 Pin (Peident n (typeOfExpr e)) e) : acc

nextsToExpr :: PackedPdefs -> Pexpr
nextsToExpr = pands . Map.foldrWithKey go []
    where go n e acc = (Peop2 Pin (Peop1 Pnext $ Peident n (typeOfExpr e)) e) : acc

readSMV :: FilePath -> IO PackedPmodule
readSMV f = do
    txt <- BS.readFile f
    parseSMV f txt

readSMVs :: [FilePath] -> IO [(Digest SHA256,PackedPmodule)]
readSMVs fs = liftM (map (id >< snd)) $ State.execStateT (mapM go fs) []
    where
    go :: FilePath -> StateT [(Digest SHA256,(FilePath,PackedPmodule))] IO ()
    go path = do
        m <- State.get
        let swapl (x,(y,z)) = (y,(x,z))
        (h,smv) <- case List.lookup path (map swapl m) of
            Just (h,smv) -> return (h,smv)
            Nothing -> do
                txt <- liftIO $ BS.readFile path
                let h = Crypto.hash $ BS.toStrict txt
                case List.lookup h m of
                    Just (_,smv) -> return (h,smv)
                    Nothing -> liftM (h,) $ liftIO $ parseSMV path txt
        State.modify $ \m -> m ++ [(h,(path,smv))]

parseSMV :: FilePath -> ByteString -> IO PackedPmodule
parseSMV f txt = do
    m <- ioErrorM $ runSMVParser f txt >>= packPmodule . L.unloc
    return $ addModuleTypes m

writeSMV ::  FilePath -> PackedPmodule -> IO ()
writeSMV f smv = do
    writeFile f $ show $ pretty smv

