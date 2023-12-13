-- 1st

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

-- 2nd

import Data.List

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)
          
freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam x m) = delete x (freeVars m)

renameVar :: String -> Expr -> Expr -> Symb
renameVar x m n
  | x `notElem` freeVars n && x `notElem` freeVars m = x
  | otherwise = renameVar (x ++ "'") n m

subst :: Symb -> Expr -> Expr -> Expr
subst v n (m :@ n') = subst v n m :@ subst v n n'
subst v n (Var x)
  | v == x = n
  | otherwise = Var x
subst v n (Lam x m)
  | v == x = Lam x m
  | x `elem` freeVars n = Lam (renameVar x n m) (subst v n (subst x (Var $ renameVar x n m) m))
  | otherwise = Lam x (subst v n m)

alphaEq :: Expr -> Expr -> Bool
alphaEq (m :@ n) (m' :@ n')
  | alphaEq m m' && alphaEq n n' = True
  | otherwise = False
alphaEq (Lam x m) (Lam y n)
  | alphaEq (subst x (Var $ renameVar x m n) m) (subst y (Var $ renameVar x m n) n) = True
  | otherwise = False
alphaEq (Var x) (Var y) = x == y
alphaEq x y = False

-- 3rd

import Data.List

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam x m) = delete x (freeVars m)

renameVar :: String -> Expr -> Expr -> Symb
renameVar x m n
  | x `notElem` freeVars n && x `notElem` freeVars m = x
  | otherwise = renameVar (x ++ "'") n m

subst :: Symb -> Expr -> Expr -> Expr
subst v n (m :@ n') = subst v n m :@ subst v n n'
subst v n (Var x)
  | v == x = n
  | otherwise = Var x
subst v n (Lam x m)
  | v == x = Lam x m
  | x `elem` freeVars n = Lam (renameVar x n m) (subst v n (subst x (Var $ renameVar x n m) m))
  | otherwise = Lam x (subst v n m)

alphaEq :: Expr -> Expr -> Bool
alphaEq (m :@ n) (m' :@ n')
  | alphaEq m m' && alphaEq n n' = True
  | otherwise = False
alphaEq (Lam x m) (Lam y n)
  | alphaEq (subst x (Var $ renameVar x m n) m) (subst y (Var $ renameVar x m n) n) = True
  | otherwise = False
alphaEq (Var x) (Var y) = x == y
alphaEq x y = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x) = Nothing
reduceOnce ((Lam x m) :@ n) = Just (subst x n m)
reduceOnce (Lam x m) = fmap (Lam x) (reduceOnce m)
reduceOnce (m :@ n) = case reduceOnce m of
  Just y -> Just (y :@ n)
  Nothing -> fmap (m :@) (reduceOnce n)

-- 4th

import Data.List
import Data.Maybe

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam x m) = delete x (freeVars m)

renameVar :: String -> Expr -> Expr -> Symb
renameVar x m n
  | x `notElem` freeVars n && x `notElem` freeVars m = x
  | otherwise = renameVar (x ++ "'") n m

subst :: Symb -> Expr -> Expr -> Expr
subst v n (m :@ n') = subst v n m :@ subst v n n'
subst v n (Var x)
  | v == x = n
  | otherwise = Var x
subst v n (Lam x m)
  | v == x = Lam x m
  | x `elem` freeVars n = Lam (renameVar x n m) (subst v n (subst x (Var $ renameVar x n m) m))
  | otherwise = Lam x (subst v n m)

alphaEq :: Expr -> Expr -> Bool
alphaEq (m :@ n) (m' :@ n')
  | alphaEq m m' && alphaEq n n' = True
  | otherwise = False
alphaEq (Lam x m) (Lam y n)
  | alphaEq (subst x (Var $ renameVar x m n) m) (subst y (Var $ renameVar x m n) n) = True
  | otherwise = False
alphaEq (Var x) (Var y) = x == y
alphaEq x y = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x) = Nothing
reduceOnce ((Lam x m) :@ n) = Just (subst x n m)
reduceOnce (Lam x m) = fmap (Lam x) (reduceOnce m)
reduceOnce (m :@ n) = case reduceOnce m of
  Just y -> Just (y :@ n)
  Nothing -> fmap (m :@) (reduceOnce n)
  
nf :: Expr -> Expr
nf x = if isNothing (reduceOnce x) then x else nf (fromJust $ reduceOnce x)

-- 5th

import Data.List
import Data.Maybe

type Symb = String 
infixl 2 :@
infix 1 `betaEq`, `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)
          
freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam x m) = delete x (freeVars m)

renameVar :: String -> Expr -> Expr -> Symb
renameVar x m n
  | x `notElem` freeVars n && x `notElem` freeVars m = x
  | otherwise = renameVar (x ++ "'") n m
  
subst :: Symb -> Expr -> Expr -> Expr
subst v n (m :@ n') = subst v n m :@ subst v n n'
subst v n (Var x)
  | v == x = n
  | otherwise = Var x
subst v n (Lam x m)
  | v == x = Lam x m
  | x `elem` freeVars n = Lam (renameVar x n m) (subst v n (subst x (Var $ renameVar x n m) m))
  | otherwise = Lam x (subst v n m)

alphaEq :: Expr -> Expr -> Bool
alphaEq (m :@ n) (m' :@ n')
  | alphaEq m m' && alphaEq n n' = True
  | otherwise = False
alphaEq (Lam x m) (Lam y n)
  | alphaEq (subst x (Var $ renameVar x m n) m) (subst y (Var $ renameVar x m n) n) = True
  | otherwise = False
alphaEq (Var x) (Var y) = x == y
alphaEq x y = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x) = Nothing
reduceOnce ((Lam x m) :@ n) = Just (subst x n m)
reduceOnce (Lam x m) = fmap (Lam x) (reduceOnce m)
reduceOnce (m :@ n) = case reduceOnce m of
  Just y -> Just (y :@ n)
  Nothing -> fmap (m :@) (reduceOnce n)

nf :: Expr -> Expr
nf x = if isNothing (reduceOnce x) then x else nf (fromJust $ reduceOnce x)

betaEq :: Expr -> Expr -> Bool
betaEq m n = alphaEq (nf m) (nf n)