module Utils.Pretty
    ( module Utils.Pretty
    , module Prettyprinter
    ) where

import Prettyprinter
import Prelude hiding ((<>))
import Data.Map (Map(..))
import qualified Data.Map as Map

sepBy o [] = emptyDoc
sepBy o [x] = x
sepBy o (x:xs) = x <+> o <+> sepBy o xs

prettyprint :: Pretty a => a -> String
prettyprint a = show $ pretty a

nestvcat :: Int -> [Doc ann] -> Doc ann
nestvcat i [] = emptyDoc
nestvcat i (x:xs) = nest i (vcat $ pretty (replicate i ' ') <> x : xs)

instance (Pretty a,Pretty b) => Pretty (Map a b) where
    pretty xs = encloseSep (pretty "{") (pretty "}") (pretty " , ") (map go $ Map.toList xs)
        where
        go (a,b) = pretty a <+> pretty "=" <+> pretty b

instance (Pretty a,Pretty b) => Pretty (Either a b) where
    pretty (Left x) = pretty "Left" <+> pretty x
    pretty (Right x) = pretty "Right" <+> pretty x