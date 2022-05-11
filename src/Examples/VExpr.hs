module Examples.VExpr (Expr (Lit, Add, VExpr), eval, veval, evalEA, evolution) where

import CC.ChoiceCalculus (V (Chc, Dim, Obj), liftV)

type VExpr = V Expr

data Expr
  = Lit Int
  | Add Expr Expr
  | VExpr VExpr
  deriving (Eq)

eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (VExpr ve) = ve >>= eval

veval :: VExpr -> V Int
veval = liftV eval

instance Show Expr where
  show (Lit int) = show int
  show (Add e1 e2) = show e1 <> " + " <> show e2
  show (VExpr vexpr) = show vexpr

type Cache = [(Expr, V Int)]

updateCache :: (Expr, V Int) -> Cache -> Cache
updateCache (e', i') [] = [(e', i')]
updateCache (e', i') ((e, i) : cs) = if e == e' then (e', i') : cs else (e, i) : updateCache (e', i') cs

inCache :: Expr -> Cache -> Maybe (V Int)
inCache _ [] = Nothing
inCache e' ((e, i) : cs) = if e == e' then Just i else e' `inCache` cs

type PreOrderLocation = Int

type ParticularEvolution = Expr -> PreOrderLocation -> Expr -> Expr

evolution :: ParticularEvolution
evolution expI loc nexp = fst (evolAux expI loc nexp)

evolAux :: Expr -> PreOrderLocation -> Expr -> (Expr, Int)
evolAux expI loc nexp = case (compare loc 0, expI) of
  (LT, _) -> error $ "Error on search: loc '" <> show loc <> "' is less than zero."
  (EQ, _) -> (nexp, 0)
  (GT, l@(Lit v)) -> (l, loc)
  (GT, Add left right) ->
    let (leftWalk, nLoc) = evolAux left (loc -1) nexp
     in if nLoc == 0
          then (Add leftWalk right, 0)
          else
            let (rightWalk, rnLoc) = evolAux right (nLoc - 1) nexp
             in (Add left rightWalk, rnLoc)
  (GT, VExpr (Obj e)) -> let (e', nLoc) = evolAux e loc nexp in (VExpr (Obj e'), nLoc)
  (GT, VExpr (Dim d t v)) -> let (e', nLoc) = evolAux (VExpr v) loc nexp in (VExpr (Dim d t (Obj e')), nLoc)
  (GT, VExpr (Chc d [])) -> error $ "Error on search: loc '" <> show loc <> "' is greater than zero, but there are no more subexpressions to search."
  (GT, VExpr (Chc d (c:cs))) -> let (c', nLoc) = evolAux (VExpr c) (loc-1) nexp in if nLoc == 0
          then (VExpr (Chc d (Obj c':cs)), 0)
          else let (c'', nLoc'') = evolAux (VExpr (Chc d cs)) (loc-1) nexp in (VExpr (Chc d (Obj c'':cs)), nLoc'')

evalEA :: Expr -> Cache -> PreOrderLocation -> Expr -> (Cache, Expr, V Int)
evalEA expI cache loc nexp =
  let evolvedModel = evolution expI loc nexp
      analysisLocalChange = eval'' cache nexp
      newCache = updateCache (nexp, analysisLocalChange) cache
   in (newCache, evolvedModel, eval'' newCache evolvedModel)

veval'' :: Cache -> VExpr -> V Int
veval'' c = liftV (eval'' c)

eval'' :: Cache -> Expr -> V Int
eval'' c e@(Lit _) = case e `inCache` c of
  Just i -> i
  Nothing -> eval e
eval'' c e@(Add e1 e2) = case e `inCache` c of
  Just i -> i
  Nothing -> (+) <$> eval'' c e1 <*> eval'' c e2
eval'' c e@(VExpr ve) = case e `inCache` c of
  Just i -> i
  Nothing -> veval'' c ve
