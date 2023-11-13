import Data.List

type Symb = String
infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

subst :: Symb -> Expr -> Expr -> Expr
subst v n m = case m of
  Var x | x == v -> n
        | otherwise -> Var x
  e1 :@ e2 -> subst v n e1 :@ subst v n e2
  Lam x e
    | x == v -> Lam x e
    | x `elem` freeVars n -> 
        let x' = generateNewVar (v : freeVars n ++ freeVars e)
        in Lam x' (subst v n (renameVar x (Var x') e))
    | otherwise -> Lam x (subst v n e)
  where
    freeVars (Var v) = [v]
    freeVars (e1 :@ e2) = freeVars e1 ++ freeVars e2
    freeVars (Lam x e) = freeVars e \\ [x]

    renameVar x replacement (Var v) | x == v = replacement
    renameVar _ _ (Var v) = Var v
    renameVar x replacement (e1 :@ e2) = renameVar x replacement e1 :@ renameVar x replacement e2
    renameVar x replacement (Lam v e) | x == v = Lam v e
    renameVar x replacement (Lam v e)
      | v `elem` freeVars replacement = renameVar x replacement (Lam newVar (renameVar v (Var newVar) e))
      | otherwise = Lam v (renameVar x replacement e)
      where
        newVar = generateNewVar (x : freeVars replacement ++ freeVars e)

    generateNewVar usedVars = head $ filter (`notElem` usedVars) (map (\c -> "x" ++ [c]) ['a'..'z'])
