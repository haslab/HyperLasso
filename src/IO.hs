module IO where

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import System.Directory
import System.FilePath

import Transform.Pexpr
import Transform.Normalize
import SMV.Syntax
import SMV.Typing
import SMV.Pretty hiding (SmvMode(..))
import qualified SMV.Pretty as SMV
import SMV.Packed
import SMV.Parser
import SMV.Trace as SMV
import Utils.Error
import Utils.Misc
import Utils.Pretty
import qualified Utils.Location as L

writeSMV :: Bool -> FilePath -> PackedPmodule -> IO ()
writeSMV isDebug f smv = do
    writeFile f $ prettyprint smv
    when isDebug $ putStrLn $ "Wrote SMV file " ++ f

withTempSMV :: Bool -> Bool -> PackedPmodule -> (FilePath -> IO a) -> IO a
withTempSMV doRemoveTemps isDebug smv go = withSystemTempUnlessError doRemoveTemps isDebug "out.smv" $ \f -> do
    writeSMV isDebug f smv
    go f

withTempSMVs :: Bool -> Bool -> [PackedPmodule] -> ([FilePath] -> IO a) -> IO a
withTempSMVs doRemoveTemps isDebug [] go = go []
withTempSMVs doRemoveTemps isDebug (smv:smvs) go = withSystemTempUnlessError doRemoveTemps isDebug "out.smv" $ \f -> do
    writeSMV isDebug f smv
    withTempSMVs doRemoveTemps isDebug smvs (go . (f:))

writeFormula :: Bool -> FilePath -> Pformula -> IO ()
writeFormula isDebug fn formula = do
    writeFile fn $ prettyprint $ normalizeFormula formula
    when isDebug $ putStrLn $ "Wrote formula file " ++ fn

withTempFormula :: Bool -> Bool -> Pformula -> (FilePath -> IO a) -> IO a
withTempFormula doRemoveTemps isDebug formula go = withSystemTempUnlessError doRemoveTemps isDebug "formula" $ \f -> do
    writeFormula isDebug f formula
    go f

readFormula :: Bool -> FilePath -> [PackedPtypes] -> IO Pformula
readFormula isDebug fn tys = do
    txt <- BS.readFile fn
    f <- ioErrorM $ runFormulaParser fn txt >>= return . L.unloc
    let qs = quantsPformula f
    when (length qs /= length tys) $ exitWithErrorMessage "Please provide same number of models and formula quantifiers"
    return $ addFormulaTypes tys f

writeTraces :: Bool -> [(String,Quant)] -> [Maybe SMV.Trace] -> IO ()
writeTraces isDebug qs traces = forM_ (zip qs traces) $ \((dim,_),mbtrace) -> forM_ mbtrace $ \trace -> do
    let f = addExtension dim "witness"
    writeFile f $ prettyprint trace
    when isDebug $ putStrLn $ "Wrote witness file " ++ f
