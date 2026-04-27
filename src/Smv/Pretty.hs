module SMV.Pretty where
    
import Prettyprinter
import Prelude hiding ((<>))
import Data.Set (Set(..))
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IntSet

import Utils.Pretty
import qualified Utils.Location as L
import SMV.Syntax
import Utils.Misc

instance Pretty a => Pretty (L.Located a) where
    pretty (L.Located _ x) = pretty x

instance Pretty Pformula where
    pretty (Pfexists n e) = pretty "exists" <+> pretty n <> pretty "." <+> pretty e
    pretty (Pfforall n e) = pretty "forall" <+> pretty n <> pretty "." <+> pretty e
    pretty (Pfltl e) = pretty "\n" <> pretty e

instance Pretty Pmodule where
    pretty (Pmodule n is) = vcat $ (pretty "MODULE" <+> ppName n) : map (pretty) is

instance Pretty Pitem where
    pretty (Pivar vs False) = vcat $ (pretty "VAR") : map (pretty) vs
    pretty (Pivar vs True) = vcat $ (pretty "FROZENVAR") : map (pretty) vs
    pretty (Pidefine ds) = vcat (pretty "DEFINE" : map (pretty) ds)
    pretty (Piinit i) = vcat [pretty "INIT",pretty i]
    pretty (Piinvar i) = vcat [pretty "INVAR",pretty i]
    pretty (Pitrans i) = vcat [pretty "TRANS",pretty i]
    pretty (Piltlspec i) = vcat [pretty "LTLSPEC",pretty i,pretty ";"]
    pretty (Piassign a) = nest 4 $ vcat (pretty "ASSIGN":map (pretty) a)
    
instance Pretty Ptype where
    pretty Pboolean = pretty "boolean"
    pretty (Pint i j) = pretty i <> pretty ".." <> pretty j
    pretty (Parray i j t) = pretty "array" <+> pretty i <> pretty ".." <> pretty j <+> pretty "of" <+> pretty t
    pretty (Penum is) = pretty '{' <> sepBy (pretty ',') (map pretty $ IntSet.toList is) <> pretty '}'

instance Pretty Pvar where
    pretty (Pvar n t) = pclose False (ppIdent n) <+> pretty ':' <+> pretty t <> pretty ';'
    
ppDims :: Pdims -> Doc ann
ppDims dims = hcat (map (\dim -> brackets (ppClosedExpr False dim)) dims)
    
instance Pretty Pdefine where
    pretty (Pdefine l r) = ppName l <+> pretty ":=" <+> pretty r <> pretty ';'

instance Pretty Passign where
    pretty (Passign l r) = pretty l <+> pretty ":=" <+> pretty r <> pretty ';'

instance Pretty Passign_lhs where
    pretty (Painit p) = pretty "init" <> (pclose True $ ppIdent p)
    pretty (Panext p) = pretty "next" <> (pclose True $ ppIdent p)

instance Pretty Popn where
    pretty Pand = pretty '&'
    pretty Por = pretty '|'
    pretty Pset = pretty ','
    pretty o = error $ "no pretty for Popn " ++ show o

instance Pretty Pop1 where
    pretty Pnot = pretty '!'
    pretty Pf = pretty 'F'
    pretty Pg = pretty 'G'
    pretty Px = pretty 'X'
    pretty Py = pretty 'Y'
    pretty Pz = pretty 'Z'
    pretty Ph = pretty 'H'
    pretty Pnext = pretty "next"
    pretty o = error $ "no pretty for Pop1 " ++ show o

instance Pretty Pop2 where
    pretty Pequiv = pretty "<->"
    pretty Pimplies = pretty "->"
    pretty Peq = pretty "="
    pretty Pneq = pretty "!="
    pretty Pplus = pretty '+'
    pretty Pminus = pretty '-'
    pretty Ptimes = pretty '*'
    pretty Pmod = pretty "mod"
    pretty Pxor = pretty "xor"
    pretty Pleq = pretty "<="
    pretty Plt = pretty "<"
    pretty Pgeq = pretty ">="
    pretty Pgt = pretty ">"
    pretty Punion = pretty "union"
    pretty Pin = pretty "in"
    pretty Pu = pretty 'U'
    pretty Pv = pretty 'V'
    pretty o = error $ "no pretty for Pop2 " ++ show o
    
instance Pretty Pexpr where
    pretty e = pclose False $ ppExpr e

data PDoc ann = PClosed Bool (Doc ann) | PDoc (Doc ann)

unPDoc :: PDoc ann -> Doc ann
unPDoc (PClosed _ p) = p
unPDoc (PDoc p) = p

pclose :: Bool -> PDoc ann -> Doc ann
pclose True (PClosed True p) = p
pclose True (PClosed False p) = parens p
pclose True (PDoc p) = parens p
pclose False (PClosed _ p) = p
pclose False (PDoc p) = parens p

ppClosedExpr :: Bool -> Pexpr -> Doc ann
ppClosedExpr withParens e = pclose withParens $ ppExpr e

ppExpr :: Pexpr -> PDoc ann
ppExpr (Peident n t) = ppIdent n
ppExpr (Pebool True) = PClosed False $ pretty "TRUE"
ppExpr (Pebool False) = PClosed False $ pretty "FALSE"
ppExpr (Peint i) = PClosed False $ pretty i
ppExpr (Peop1 Pnext e) = PDoc $ pretty Pnext <+> ppClosedExpr True e
ppExpr (Peop1 o e) = PDoc $ pretty o <+> ppClosedExpr False e
ppExpr (Peop2 o e1 e2) = PDoc $ ppClosedExpr False e1 <+> pretty o <+> ppClosedExpr False e2
ppExpr (Peopn Pset es) = PClosed False $ braces $ sepBy (pretty Pset) (map (ppClosedExpr False) es)
ppExpr (Peopn o es) = PDoc $ sepBy (pretty o) (map (ppClosedExpr False) es)
ppExpr (Pecase cases) = PClosed False $ nest 4 $ vcat (pretty "case":map printCase cases ++ [pretty "esac"])
    where
    printCase (l,r) = ppClosedExpr True l <+> pretty ":" <+> pretty r <> pretty ';'
ppExpr (Pedemorgan c e1 e2) = PDoc $ ppClosedExpr False c <+> pretty '?' <+> ppClosedExpr False e1 <+> pretty ':' <+> ppClosedExpr False e2 
      
instance Pretty Pident where
    pretty = pclose False . ppIdent
      
prettyPident :: Pident -> String
prettyPident = show . pclose False . ppIdent
      
ppIdent :: Pident -> PDoc ann
ppIdent (Pident n dims) = ppIdent' n dims

ppIdent' :: String -> Pdims -> PDoc ann
ppIdent' n dims = PClosed False $ ppName n <> ppDims dims
ppIdent' n dims = error $ "ppIdent': " ++ " " ++ show n ++ " " ++ show dims

ppName :: String -> Doc ann
ppName n = pretty n

