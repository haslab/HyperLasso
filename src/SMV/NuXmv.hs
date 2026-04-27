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