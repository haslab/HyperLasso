module Transform.Normalize where

import Data.Set(Set(..))
import qualified Data.Set as Set
import Data.HashSet(HashSet(..))
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import Data.List as List

import Utils.Pretty
import Utils.Misc
import SMV.Syntax
import SMV.Pretty
import SMV.Typing
import Transform.Pexpr

outerNext :: Pexpr -> Pexpr
outerNext e = mknext (go e)
    where
    mknext (e@(Pebool {}),isNext) = e
    mknext (e@(Peint {}),isNext) = e
    mknext (e,isNext) = if isNext then pnext e else e
    go :: Pexpr -> (Pexpr,Bool)
    go e@(Peident {}) = (e,False)
    go e@(Pebool {}) = (e,True)
    go e@(Peint {}) = (e,True)
    go (Peop1 Pnext e) = (e,True)
    go (Peop1 o e1) | isLTLOp1 o = (Peop1 o $ outerNext e1,False)
    go (Peop1 o e1) = 
        let (e1',isNext1) = go e1 in
        (Peop1 o e1',isNext1) 
    go (Peop2 o e1 e2) | isLTLOp2 o = (Peop2 o (outerNext e1) (outerNext e2),False)
    go (Peop2 o e1 e2) =
        let r1@(e1',isNext1) = go e1 in
        let r2@(e2',isNext2) = go e2 in 
        if isNext1 && isNext2 then (Peop2 o e1' e2',True) else (Peop2 o (mknext r1) (mknext r2),False)
    go (Peopn o es) = 
        let rs = map go es in
        let (es',isNexts) = unzip rs in
        if all id isNexts then (Peopn o es',True) else (Peopn o $ map mknext rs,False)
--    go e@(Peproj {}) = (e,False)
    go e@(Pecase cs) =
        let (ls,rs) = unzip $ map (id >< go) cs in
        let (es,isNexts) = unzip rs in
        if all id isNexts then (Pecase $ zip ls es,True) else (Pecase $ zip ls (map mknext rs),False)
    go e@(Pedemorgan c e1 e2) = 
        let r1@(e1',isNext1) = go e1 in
        let r2@(e2',isNext2) = go e2 in
        if isNext1 && isNext2 then (Pedemorgan c e1' e2',True) else (Pedemorgan c (mknext r1) (mknext r2),False)

-- innerNext needs to be called first, otherwise unfolding case expressions is unsound
normalizeExpr :: Pexpr -> Pexpr
normalizeExpr = evaluateExpr . nnfExpr . unfoldExpr . innerNext

normalizeFormula :: Pformula -> Pformula
normalizeFormula (Pfforall n f) = Pfforall n $ normalizeFormula f
normalizeFormula (Pfexists n f) = Pfexists n $ normalizeFormula f
normalizeFormula (Pfltl e) = Pfltl $ normalizeExpr e

innerNext :: Pexpr -> Pexpr
innerNext (vnext -> Just e@(Pebool {})) = e
innerNext (vnext -> Just e@(Peint {})) = e
innerNext (vnext -> Just (Peop1 o e1)) = Peop1 o $ innerNext $ pnext e1
innerNext (vnext -> Just (Peop2 o e1 e2)) = Peop2 o (innerNext $ pnext e1) (innerNext $ pnext e2)
innerNext (vnext -> Just (Peopn o es)) = Peopn o $ map (innerNext . pnext) es
innerNext (vnext -> Just (Pecase cs)) = Pecase $ map (id >< (innerNext . pnext)) cs
innerNext (vnext -> Just (Pedemorgan c e1 e2)) = Pedemorgan c (innerNext $ pnext e1) (innerNext $ pnext e2)
innerNext e@(Pebool {}) = e
innerNext e@(Peint {}) = e
innerNext e@(Peident {}) = e
innerNext (Peop1 o e1) = Peop1 o (innerNext e1)
innerNext (Peop2 o e1 e2) = Peop2 o (innerNext e1) (innerNext e2)
innerNext (Peopn o es) = Peopn o (map innerNext es)
innerNext (Pecase cs) = Pecase $ map (id >< innerNext) cs
innerNext (Pedemorgan c e1 e2) = Pedemorgan c (innerNext e1) (innerNext e2)

unfoldExpr :: Pexpr -> Pexpr
unfoldExpr (Peop2 Pimplies e1 e2) = unfoldExpr $ unfoldImplies e1 e2
unfoldExpr (Peop2 Pin e1 (vset -> Just is)) | List.null is = Pebool False
unfoldExpr (Peop2 Pin e1 (vset -> Just [e2])) = unfoldExpr $ Peop2 Peq e1 e2
unfoldExpr (Peop2 Pin e1 e2) | isConstantExpr e2 = unfoldExpr $ Peop2 Peq e1 e2
unfoldExpr (Peop2 Pin e1 (vsetbool -> Just bs)) = case HashSet.size bs of
    0 -> Pebool False
    1 -> if popHashSet bs then unfoldExpr e1 else unfoldExpr (pnot e1) 
    2 -> Pebool True
unfoldExpr (Peop2 Pin e1@(vcase -> Just cs1) (vset -> Just is)) = unfoldExpr $ pors $ map (Peop2 Peq e1) is
unfoldExpr (Peop2 Peq e1 e2) | isBoolExpr e1 && isBoolExpr e2 = unfoldExpr $ Peop2 Pequiv e1 e2 --unfoldExpr $ (e1 `pand` e2) `por` (pnot e1 `pand` pnot e2)
unfoldExpr e@(Pebool {}) = e
unfoldExpr e@(Peint {}) = e
unfoldExpr e@(Peident {}) = e
unfoldExpr (Peop1 o e1) = Peop1 o (unfoldExpr e1)
unfoldExpr (Peop2 Punion e1 e2) = unfoldExpr $ Peopn Pset $ joinUnions [e1,e2]
unfoldExpr e@(Peop2 o e1 (vcase -> Just cs2)) | isBoolExpr e = unfoldExpr $ inlineCaseExprBool $ map (id >< peop2 o e1) cs2
unfoldExpr e@(Peop2 o (vcase -> Just cs1) e2) | isBoolExpr e = unfoldExpr $ inlineCaseExprBool $ map (id >< (\e1 -> peop2 o e1 e2)) cs1
unfoldExpr (Peop2 o e1 e2) = peop2 o (unfoldExpr e1) (unfoldExpr e2)
unfoldExpr (Peopn o es) = peopn o (map unfoldExpr es)
unfoldExpr e@(Peop2 o@(isCmpOp2 -> True) e1 (Pecase cs2)) | isBoolExpr e = unfoldExpr $ fst $ foldl caseOp (pfalse,pfalse) cs2
    where caseOp (acc,pre) (c2,e2) = (por acc $ pands $ [pnot pre,c2,peop2 o e1 e2],por pre c2)
unfoldExpr e@(Peop2 o@(isCmpOp2 -> True) e1@(Pecase cs1) e2) = unfoldExpr $ Peop2 (negCmpOp2 o) e2 e1
unfoldExpr e@(Pecase cs) | isIntExpr e = unfoldExpr $ unfoldIntCaseExpr cs
unfoldExpr e@(Pecase cases) | isBoolExpr e = unfoldExpr $ inlineCaseExprBool cases
unfoldExpr e@(Pecase cs) = error $ "unfoldExpr: " ++ prettyprint e --Pecase $ map (unfoldExpr >< unfoldExpr) cs
unfoldExpr (Pedemorgan c te fe) = unfoldExpr $ Pecase [(c,te),(pnot c,fe)]
unfoldExpr (Peop2 Peq e1@(isBoolExpr -> True) e2@(isBoolExpr -> True)) = unfoldExpr $ peop2 Pequiv e1 e2 

unfoldEquiv e1 e2 = (e1 `pand` e2) `por` (pnot e1 `pand` pnot e2)
unfoldImplies e1 e2 = pnot e1 `por` e2

joinUnions :: [Pexpr] -> [Pexpr]
joinUnions [] = []
joinUnions (Peop2 Punion x1 x2:xs) = joinUnions (x1 : x2 : xs)
joinUnions (x:xs) = x : joinUnions xs

nnfExpr :: Pexpr -> Pexpr    
nnfExpr (vnotnot -> Just e) = nnfExpr e
nnfExpr (vnotors -> Just es) = pands $ map (nnfExpr . pnot) es
nnfExpr (vnotands -> Just es) = pors $ map (nnfExpr . pnot) es
nnfExpr (vnot -> Just (Peop2 Pimplies e1 (Pebool False))) = nnfExpr e1
nnfExpr (vnot -> Just (Peop2 Pimplies e1 e2)) = nnfExpr $ e1 `pand` pnot e2
nnfExpr (vnot -> Just (Peop1 Pf e1)) = nnfExpr $ Peop1 Pg $ pnot e1
nnfExpr (vnot -> Just (Peop1 Pg e1)) = nnfExpr $ Peop1 Pf $ pnot e1
nnfExpr (vnot -> Just (Peop1 Px e1)) = nnfExpr $ Peop1 Px $ pnot e1
nnfExpr (vnot -> Just (Peop2 Pu e1 e2)) = nnfExpr $ peop2 Pv (pnot e1) (pnot e2)
nnfExpr (vnot -> Just (Peop2 Pv e1 e2)) = nnfExpr $ peop2 Pu (pnot e1) (pnot e2)
nnfExpr (vnot -> Just (Peop2 o e1 e2)) | isCmpOp2 o = nnfExpr $ peop2 (negCmpOp2 o) e1 e2
nnfExpr (vnot -> Just e1) = case nnfExpr e1 of
    Pebool b -> Pebool $ not b
    e1' -> pnot e1'
nnfExpr e@(Peop1 o e1) = case (o,nnfExpr e1) of
    (Pf,Pebool b) -> Pebool b
    (Pg,Pebool b) -> Pebool b
    (o,e1') -> Peop1 o e1'
nnfExpr (Peop2 Peq (Pebool b1) (Pebool b2)) = Pebool (b1==b2)
nnfExpr (Peop2 Peq (Peint i1) (Peint i2)) = Pebool (i1==i2)
nnfExpr e@(Peop2 o e1 (Pecase cs2)) | isBoolExpr e = nnfExpr $ fst $ foldl caseOp (pfalse,pfalse) cs2
    where caseOp (acc,pre) (c2,e2) = (por acc $ pands $ [pnot pre,c2,peop2 o e1 e2],por pre c2)
nnfExpr e@(Peop2 o (Pecase cs1) e2) | isBoolExpr e = nnfExpr $ fst $ foldl caseOp (pfalse,pfalse) cs1
    where caseOp (acc,pre) (c1,e1) = (por acc $ pands $ [pnot pre,c1,peop2 o e1 e2],por pre c1)
--nnfExpr (Peop2 Pimplies e1 e2) | breakImplies = nnfExpr $ pnot e1 `por` e2
--nnfExpr (Peop2 Pequiv e1 e2) | breakEquiv = nnfExpr $ (e1 `pand` e2) `por` (pnot e1 `pand` pnot e2)
nnfExpr (Peop2 Peq e1@(isBoolExpr -> True) e2@(isBoolExpr -> True)) = nnfExpr $ peop2 Pequiv e1 e2 --nnfExpr $ (e1 `pand` e2) `por` (pnot e1 `pand` pnot e2)
nnfExpr e@(Pecase cs) | isIntExpr e = nnfExpr $ unfoldIntCaseExpr cs
nnfExpr (Peop2 Pimplies (Pebool True) e2) = nnfExpr e2
nnfExpr (Peop2 Pimplies e1 (Pebool False)) = nnfExpr $ pnot e1
nnfExpr (Peop2 Pequiv e1 (Pebool False)) = nnfExpr $ pnot e1
nnfExpr (Peop2 Pequiv e1 (Pebool True)) = nnfExpr e1
nnfExpr (Peop2 Pequiv (Pebool False) e2) = nnfExpr $ pnot e2
nnfExpr (Peop2 Pequiv (Pebool True) e2) = nnfExpr e2
nnfExpr (Peop2 Pimplies (Pebool False) e2) = Pebool True
nnfExpr (Peop2 Pimplies e1 (Pebool True)) = Pebool True
nnfExpr e@(Peident n t) = e
nnfExpr e@(Pebool _) = e
nnfExpr e@(Peint _) = e
nnfExpr e@(Peop2 o e1 e2) = Peop2 o (nnfExpr e1) (nnfExpr e2)
nnfExpr (Peopn o es) = peopn o $ map nnfExpr es
nnfExpr (Pecase cs) = Pecase $ map (\(x,y) -> (nnfExpr x,nnfExpr y)) cs
nnfExpr (Pedemorgan c e1 e2) = Pecase [(nnfExpr c,nnfExpr e1),(ptrue,nnfExpr e2)]

unfoldIntCaseExpr :: [(Pexpr,Pexpr)] -> Pexpr
unfoldIntCaseExpr [] = Peint 0
unfoldIntCaseExpr ((l,r):xs) = (pcast EInt l `ptimes` r) `pplus` ((pcast EInt $ pnot l) `ptimes` unfoldIntCaseExpr xs)

evaluateExpr :: Pexpr -> Pexpr
evaluateExpr (Peop1 o e1) =
    case (o,evaluateExpr e1) of
        (Pnot,Pebool b) -> Pebool $ not b
        (Pf,Pebool b) -> Pebool b
        (Pg,Pebool b) -> Pebool b
        (Px,Pebool b) -> Pebool b
        (o,e1') -> Peop1 o e1'
evaluateExpr (Peop2 o e1 e2) = 
    case (o,evaluateExpr e1,evaluateExpr e2) of
        (Pequiv,e1,Pebool False) -> nnfExpr $ pnot e1
        (Pequiv,e1,Pebool True) -> e1
        (Pequiv,Pebool False,e2) -> nnfExpr $ pnot e1
        (Pequiv,Pebool True,e2) -> e2
        (Peq,Peint i,Peint j) -> Pebool (i==j)
        (Pneq,Peint i,Peint j) -> Pebool (i/=j)
        (Pgt,Peint i,Peint j) -> Pebool (i>j)
        (Pgeq,Peint i,Peint j) -> Pebool (i>=j)
        (Plt,Peint i,Peint j) -> Pebool (i<j)
        (Pleq,Peint i,Peint j) -> Pebool (i<=j)
        (o,e1',e2') -> Peop2 o e1' e2'
evaluateExpr e@(Peident _ t) = e
evaluateExpr e@(Pebool _) = e
evaluateExpr e@(Peint _) = e
evaluateExpr (Peopn Pand es) = pands (map evaluateExpr es)
evaluateExpr (Peopn Por es) = pors (map evaluateExpr es)
evaluateExpr (Peopn Pset es) = pset (map evaluateExpr es)
evaluateExpr (Pecase cs) = Pecase $ map (evaluateExpr >< evaluateExpr) cs
evaluateExpr (Pedemorgan (Pebool b) e1 e2) = if b then evaluateExpr e1 else evaluateExpr e2
evaluateExpr e = e



