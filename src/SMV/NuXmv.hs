-- |
-- Driver for the external @nuXmv@ model checker, used as the complete
-- (non-hyper) LTL backend that validates counter-example candidates
-- synthesised by the SMT loop in "MC".
--
-- The backend is invoked as a subprocess via @shelly@; the @nuXmv@ binary
-- must be on @PATH@. Two operations are exposed and form the interface
-- that any replacement backend (e.g. a different complete LTL checker)
-- should implement:
--
--   * 'doCheckLTLSpecNuXMV' — check an LTL spec against an SMV file,
--     returning a 'Trace' counter-example if the spec fails.
--   * 'doCheckNonEmptyNuXMV' — check whether the model admits any
--     infinite execution.
--
-- Known hard-coded prototype assumptions documented for reviewers and
-- downstream extenders:
--
--   * The BMC bound for the non-emptiness check is fixed at @-k 99@ and
--     the fairness probe is fixed at @F FALSE@ in
--     'nuXmvCheckNonEmptyScript'. The match string used to detect "no
--     counter-example" in 'doCheckNonEmptyNuXMV' must be kept in sync if
--     the bound is changed.
--   * The nuXmv command pipeline (@read_model; flatten_hierarchy;
--     encode_variables; build_model; …@) is hard-coded in
--     'nuXmvCheckLTLSpecScript' / 'nuXmvCheckNonEmptyScript'.
--
-- See @ARCHITECTURE.md@ at the repo root.
module SMV.NuXmv where

import Data.Maybe
import Data.List as List
import qualified Data.Text as T
import Data.Text (Text(..))
import Shelly

import SMV.Trace
import Utils.Misc

doCheckLTLSpecNuXMV :: Bool -> FilePath -> IO (Maybe Trace)
doCheckLTLSpecNuXMV isDebug infile = Shelly.shelly $ shellyMode isDebug $ do
    Shelly.setStdin $ nuXmvCheckLTLSpecScript infile
    out <- Shelly.run "nuXmv" ["-int"] 
    if (T.isInfixOf "is true" out)
        then return Nothing
        else return $ Just $ parseTrace (pruneNuXmv $ T.unpack out)

doCheckNonEmptyNuXMV :: Bool -> FilePath -> IO Bool
doCheckNonEmptyNuXMV isDebug infile = Shelly.shelly $ shellyMode isDebug $ do
    Shelly.setStdin $ nuXmvCheckNonEmptyScript infile
    out <- Shelly.run "nuXmv" ["-int"] 
    if (T.isInfixOf "no counterexample found with bound 99" out)
        then return False
        else return True
    
pruneNuXmv :: String -> String
pruneNuXmv str = unlines $ catMaybes $ map pruneLine $ lines str
    where
    pruneLine str | isPrefixOf "nuXmv" str = Nothing
    pruneLine str | isPrefixOf "***" str = Nothing
    pruneLine str | isPrefixOf "--" str = Nothing
    pruneLine "" = Nothing
    pruneLine str = Just str
    
nuXmvCheckLTLSpecScript :: FilePath -> Text
nuXmvCheckLTLSpecScript infile = T.unlines
    ["read_model -i " <> T.pack infile <> ";"
    ,"flatten_hierarchy;"
    ,"encode_variables;"
    ,"build_model;"
    ,"check_ltlspec;"
    ,"quit;"]
    
nuXmvCheckNonEmptyScript :: FilePath -> Text
nuXmvCheckNonEmptyScript infile = T.unlines
    ["read_model -i " <> T.pack infile <> ";"
    ,"flatten_hierarchy;"
    ,"encode_variables;"
    ,"build_model;"
    ,"set on_failure_script_quits;"
    ,"go_bmc;"
    ,"check_ltlspec_bmc_inc -k 99 -l * -p \"F FALSE\";"
    ,"quit;"]