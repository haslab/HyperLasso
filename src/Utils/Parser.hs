module Utils.Parser where

import Text.Parsec (Parsec(..),ParsecT(..),Stream(..))
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec
import qualified Text.Parsec.Language as Parsec
import Data.List as List
import Control.Monad

parseString :: Parser a -> String -> Either Parsec.ParseError a
parseString parser str = Parsec.parse parser "" str

parseStrings :: LineParser a -> String -> Either Parsec.ParseError a
parseStrings parser str = Parsec.parse parser "" (lines str)

lexer = Parsec.makeTokenParser Parsec.emptyDef

sepByParser :: Parsec s u a -> Parsec s u b -> Parsec s u [a]
sepByParser p s = go <||> (return [])
    where
    go = do
        x <- p
        continue x <||> (return [x])
    continue x = do
        s
        xs <- sepByParser p s
        return (x:xs)

eitherParser :: Parsec s u a -> Parsec s u b -> Parsec s u (Either a b)
eitherParser l r = (liftM Left l) <||> (liftM Right r)

parensParser :: Parser a -> Parser a
parensParser p = (Parsec.char '(' *> p <* Parsec.char ')') Parsec.<?> "parens"

bracesParser :: Parser a -> Parser a
bracesParser p = (Parsec.char '{' *> p <* Parsec.char '}') Parsec.<?> "braces"

stringLiteralParser :: Parser String
stringLiteralParser = (Parsec.stringLiteral lexer) Parsec.<?> "string literal"

negIntParser :: Parser (Int,Bool)
negIntParser =
    (liftM (,False) (Parsec.char '-' *> intParser))
    Parsec.<|>
    (liftM (,True) intParser)

intParser :: Parser Int
intParser = (fromInteger <$> Parsec.decimal lexer) Parsec.<?> "int"

integerParser :: Parser Integer
integerParser = (Parsec.integer lexer) Parsec.<?> "integer"

many1Till :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = (do
  first <- p
  rest  <- Parsec.manyTill p end
  return (first : rest)) Parsec.<?> "many1Till"
  
(<||>) :: Parsec s u a -> Parsec s u a -> Parsec s u a
x <||> y = (Parsec.try x) Parsec.<|> y

maybeParser :: Parsec s u a -> Parsec s u (Maybe a)
maybeParser p = liftM Just p <||> return Nothing

manyTill :: Parsec s u a -> Parsec s u end -> Parsec s u ([a],end)
manyTill p end = (liftM ([],) end) <||> (p >>= \x -> manyTill p end >>= \(xs,e) -> return (x:xs,e))

manyTill1 :: Parsec s u a -> Parsec s u end -> Parsec s u ([a],end)
manyTill1 p e = do
    x <- p
    (xs,end) <- manyTill p e
    return (x:xs,end)

bracketsParser :: Parser a -> Parser a
bracketsParser p = (Parsec.char '[' *> p <* Parsec.char ']') Parsec.<?> "brackets"

simpleStringLiteralParser :: Parser String
simpleStringLiteralParser = (aspa *> Parsec.manyTill Parsec.anyChar aspa) Parsec.<?> "string literal"
    where aspa = Parsec.char '\"'

hspace :: Parser Char
hspace = (Parsec.satisfy isHspace) Parsec.<?> "horizontal space"

isHspace :: Char -> Bool
isHspace c = List.elem c " \t"

hspaces :: Parser ()
hspaces = (Parsec.skipMany hspace) Parsec.<?> "horizontal spaces"

hspaces1 :: Parser ()
hspaces1 = (Parsec.skipMany1 hspace) Parsec.<?> "horizontal spaces"

boundedParser :: Int -> Parsec s u a -> Parsec s u [a]
boundedParser i p | i <= 0 = return []
boundedParser i p = do
    x <- p
    xs <- boundedParser (i-1) p
    return (x:xs)

removeHspace :: Parser a -> Parser a
removeHspace m = do
    inp <- Parsec.getInput
    Parsec.setInput $ filter (not . isHspace) inp
    m

parseLines :: LineParser a -> Parser a
parseLines parser = do
    inp <- Parsec.getInput
    let ls = lines inp
    case Parsec.parse parser "" ls of
        Left err -> Parsec.unexpected (show err)
        Right a -> return a

lineParser :: Parser String
lineParser = liftM fst $ manyTill Parsec.anyChar (Parsec.try $ (nullParser $ Parsec.endOfLine) <||> Parsec.eof)

wordParser :: Parser String
wordParser = liftM fst $ manyTill Parsec.anyChar (Parsec.try $ (nullParser Parsec.space) <||> Parsec.eof)

nullParser :: Parser a -> Parser ()
nullParser m = m >> return ()

negParser :: Show a => Parser a -> Parser b
negParser m = do
    x <- m
    Parsec.unexpected (show x)

type LineParser = Parsec.Parsec [String] ()
