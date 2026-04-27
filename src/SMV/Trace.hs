module SMV.Trace where

import Prettyprinter
import Data.Map (Map(..))
import qualified Data.Map as Map
import GHC.Generics
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.List as List
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IntSet
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.HashMap.Lazy (HashMap(..))
import qualified Data.HashMap.Lazy as HashMap
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.Expr as Parsec
import Text.Parsec (Parsec,(<|>))
import Control.Monad
import Data.Maybe
import Data.Char as Char
import Control.DeepSeq

import Utils.Misc
import Utils.Error
import Utils.Parser
import Utils.Pretty
import Transform.Substitute
import SMV.Syntax
import SMV.Typing
import SMV.Parser
import Transform.Pexpr
import Transform.Bexpr

data Trace = Trace { trace_description :: String, trace_type :: TraceType, trace_states :: [State] }
    deriving (Eq,Ord,Show,Generic)

instance NFData Trace

data TraceType = Example | Counterexample
    deriving (Eq,Ord,Show,Generic)

instance NFData TraceType

-- if a state is the target of a loop from the last reported state
type IsLoopTarget = Bool

data State = State { state_name :: String, state_loop :: IsLoopTarget, state_vars :: Subst }
    deriving (Eq,Ord,Show,Generic)

instance NFData State

instance Pretty TraceType where
    pretty Example = pretty "Example"
    pretty Counterexample = pretty "Counterexample"

instance Pretty Trace where
    pretty (Trace desc ty sts) = vcat [pretty "Trace Description:" <+> pretty desc , pretty "Trace Type:" <+> pretty ty , nestvcat 2 (concatMap prettyState sts)]

prettyState (State name loop vars) = ploop ++ [pst , nestvcat 2 pvs]
    where
    ploop = if loop then [pretty "-- Loop starts here"] else []
    pst = pretty "-> State:" <+> pretty name <+> pretty "<-"
    pvs = map drawState $ Map.toList vars
    drawState (n,e) = pretty n <+> pretty "=" <+> pretty e
    
traceLength :: Trace -> Int
traceLength (Trace _ _ sts) = length sts
    
traceToLTLSpec :: Trace -> Pexpr
traceToLTLSpec tr = pands $ map go $ zip [0..] $ trace_states tr
    where
    go :: (Int,State) -> Pexpr
    go (i,State _ _ vs) = nexts i $ pands $ map (\(n,e) -> Peop2 Peq (Peident n (typeOfExpr e)) e) $ Map.toList vs
    nexts :: Int -> Pexpr -> Pexpr
    nexts i e | i <= 0 = e
    nexts i e = Peop1 Px $ nexts (i-1) e
    
checkStaticTrace :: Bool -> Trace -> Maybe Trace
checkStaticTrace withLoops t@(Trace desc ty sts) = if any isFreeState sts then Nothing else if hasLoops then Just t else Nothing
    where
    hasLoops = if withLoops then any state_loop sts else True
    isFreeState :: State -> Bool
    isFreeState s = any isFreeExpr (state_vars s)    

filterTrace :: Set Pident -> Trace -> Trace
filterTrace vs tr = tr { trace_states = map filterState $ trace_states tr }
    where
    filterState :: State -> State
    filterState st = st { state_vars = filterSubst $ state_vars st }
    filterSubst :: Subst -> Subst
    filterSubst ss = Map.filterWithKey (\k _ -> Set.member k vs) ss

varsTrace :: Trace -> Set Pident
varsTrace tr = Set.unions $ map (Map.keysSet . state_vars) $ trace_states tr

composeSubstTrace :: Subst -> Trace -> Trace
composeSubstTrace ss (Trace desc ty sts) = Trace desc ty (map composeState sts)
    where
    composeState (State name loop vars) = State name loop (composeSubst ss vars)

addDimTrace :: String -> Trace -> Trace
addDimTrace dim (Trace desc ty sts) = Trace desc ty (map addDimState sts)
    where
    addDimState (State name loop vars) = State name loop (mapWithKey (\n -> addDimPident n $ mkQuantDim dim) id vars)
    
-- * parser

data TraceToken
    = T_TraceDescription
    | T_TraceType
    | T_Loop
    | T_FwdArrow
    | T_BwdArrow
    | T_State
    | T_Equals
    | T_String String
    deriving (Eq,Show)
    
instance Pretty TraceToken where
    pretty T_TraceDescription = pretty "Trace Description:"
    pretty T_TraceType = pretty "Trace Type:"
    pretty T_Loop = pretty "-- Loop starts here"
    pretty T_FwdArrow = pretty "->"
    pretty T_BwdArrow = pretty "<-"
    pretty T_State = pretty "State:"
    pretty T_Equals = pretty "="
    pretty (T_String s) = pretty s
    
traceLexer :: Parser [TraceToken]
traceLexer = do
    toks <- Parsec.sepEndBy traceTokenLexer Parsec.spaces
    Parsec.eof
    return toks

traceTokenLexer :: Parser TraceToken
traceTokenLexer = do
    (keywords <||> (liftM T_String $ stringParser))
    <|>
    dashes
    <|>
    (Parsec.string "<-" >> return T_BwdArrow)
    <|>
    (Parsec.string "=" >> return T_Equals)
  where
    keywords = traces <|> (Parsec.string "State:" >> return T_State)
    traces = do
        Parsec.string "Trace"
        Parsec.space
        (Parsec.string "Description:" >> return T_TraceDescription) <|> (Parsec.string "Type:" >> return T_TraceType)
    dashes = do
        Parsec.string "-"
        (Parsec.string ">" >> return T_FwdArrow) <|> (Parsec.string "- Loop starts here" >> return T_Loop)
    
stringParser :: Parser String
stringParser = do
    c <- Parsec.satisfy safeStartingChar
    (cs) <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead $ nullParser (Parsec.satisfy (not . safeChar)) <|> Parsec.eof)
    return $ stripString (c:cs)

safeChar :: Char -> Bool
safeChar c = not (List.elem c "<>-=\n")

safeStartingChar :: Char -> Bool
safeStartingChar c = not (isSpace c) && not (List.elem c "<>-=")
    
lexTrace :: String -> Either Parsec.ParseError [TraceToken]
lexTrace str = Parsec.parse traceLexer "" str
    
type TraceParser = Parsec.Parsec [TraceToken] ()

nextTraceToken :: Parsec.SourcePos -> t -> s -> Parsec.SourcePos
nextTraceToken pos _ _ = Parsec.incSourceColumn pos 1

traceTokenParser :: TraceToken -> TraceParser ()
traceTokenParser t = do
    Parsec.tokenPrim prettyprint nextTraceToken (\tk -> if tk==t then Just t else Nothing)
    return ()

traceStringParser :: TraceParser String
traceStringParser = Parsec.tokenPrim prettyprint nextTraceToken parse
    where
    parse (T_String str) = Just str
    parse _ = Nothing

traceTypeParser :: Parser TraceType
traceTypeParser =
    (Parsec.string "Example" >> return Example)
    <|>
    (Parsec.string "Counterexample" >> return Counterexample)

traceParser :: TraceParser Trace
traceParser = do
    traceTokenParser T_TraceDescription
    desc <- traceStringParser
    traceTokenParser T_TraceType
    tystr <- traceStringParser
    ty <- case parseString traceTypeParser tystr of
        Left err -> Parsec.unexpected (show err)
        Right ty -> return ty
    (sts) <- Parsec.manyTill stateParser (Parsec.try $ Parsec.lookAhead Parsec.eof)
    return $ Trace desc ty sts
    
stateHeaderParser :: TraceParser String
stateHeaderParser = do
    traceTokenParser T_FwdArrow
    traceTokenParser T_State
    name <- traceStringParser
    traceTokenParser T_BwdArrow
    return name
    
stateParser :: TraceParser State
stateParser = do
    loop <- maybeParser $ traceTokenParser T_Loop
    name <- stateHeaderParser
    (vars) <- Parsec.manyTill varParser (Parsec.try $ Parsec.lookAhead $ traceTokenParser T_Loop Parsec.<|> traceTokenParser T_FwdArrow Parsec.<|> Parsec.eof)
    return $ State name (isJust loop) (Map.fromList vars)

varParser :: TraceParser (Pident,Pexpr)
varParser = do
    var <- traceStringParser
    traceTokenParser T_Equals
    val <- traceStringParser
    case parsePident var of
        Nothing -> Parsec.unexpected $ "parsePident " ++ show var
        Just n -> case parseExpr val of
            Nothing -> Parsec.unexpected $ "parseExpr" ++ show val
            Just e -> return (n,e) 

parseTrace :: String -> Trace
parseTrace str = 
    case lexTrace str of
        Left err -> error $ "Error lexing trace: " ++ show err ++ "\nInput:\n" ++ str
        Right toks -> case Parsec.parse traceParser "" toks of
            Left err -> error $ "Error parsing trace: " ++ show err ++ "\nInput:\n" ++ str
            Right parsed -> parsed

-- LTL

traceToBLTL :: VarTypes -> Trace -> Bexpr
traceToBLTL tys (Trace _ _ sts) = bandsList $ map buildState ists
    where
    lastIndex = last $ map fst ists
    loopIndex = fst $ head $ filter (state_loop . snd) ists
    loopTransitions :: [(Int,Int)]
    loopTransitions = (lastIndex,loopIndex) : [ (i,i+1) | i <- [loopIndex..lastIndex-1] ]
    ists = zip [0..] sts
    substToExpr :: Subst -> Bexpr
    substToExpr ss = bandsList $ map (\(n,e) -> Bop2 Peq (mkVar n) (toBexprPure e)) $ Map.toList ss
    mkVar :: Pident -> Bexpr
    mkVar n = Bvar n (unsafeLookupNote "traceToLTL" n tys)
    stateToExpr :: State -> Bexpr
    stateToExpr = substToExpr . state_vars
    buildState :: (Int,State) -> Bexpr
    buildState (i,st) = if i == lastIndex
        then nexts i $ stateToExpr st `band` (bg $ borsList $ map buildTransition loopTransitions)
        else nexts i $ stateToExpr st
    nexts :: Int -> Bexpr -> Bexpr
    nexts i e = if i <= 0 then e else bx (nexts (pred i) e)
    buildTransition :: (Int,Int) -> Bexpr
    buildTransition (i,j) = stateToExpr (sts !! i) `band` bx (stateToExpr (sts !! j))




