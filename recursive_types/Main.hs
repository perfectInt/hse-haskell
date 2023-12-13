-- 1st

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

data B x = Empty | Zero x | One x
  deriving (Eq, Show)

type Bin = Fix B

instance Functor B where
  fmap _ Empty = Empty
  fmap f (Zero x) = Zero $ f x
  fmap f (One x) = One $ f x

phiB :: B Int -> Int
phiB Empty = 0
phiB (One x) = x * 2 + 1
phiB (Zero x) = x * 2

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB x
  | even x = Zero $ x `div` 2
  | otherwise = One $ x `div` 2

int2bin :: Int -> Bin
int2bin = ana psiB

-- 2nd

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

instance Functor E where
  fmap _ (Num a) = Num a
  fmap f (Add x y) = Add (f x) (f y)
  fmap f (Mult x y) = Mult (f x) (f y)

phiE :: E Int -> Int
phiE (Num a) = a
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a) = show (phiE $ Num a)
phiEShow (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
phiEShow (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"

-- 3rd

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

instance Functor E where
  fmap _ (Num a) = Num a
  fmap f (Add x y) = Add (f x) (f y)
  fmap f (Mult x y) = Mult (f x) (f y)

phiE :: E Int -> Int
phiE (Num a) = a
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a) = show (phiE $ Num a)
phiEShow (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
phiEShow (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num a) = (show (phiE $ Num a) ++)
phiEShowS (Add x y) = (("+ " ++ x [] ++ " " ++ y []) ++)
phiEShowS (Mult x y) = (("* " ++ x [] ++ " " ++ y []) ++)

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num a) s = push a s
phiE' (Add x y) s = push (head (add $ x [] ++ y [])) s
phiE' (Mult x y) s = push (head (mult $ x [] ++ y [])) s

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

-- 4th

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

data T a x = Leaf | Branch x a x

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch l x r) = l + x + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

-- 5th

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

data T a x = Leaf | Branch x a x deriving (Show, Eq)

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a] -- [a] -> T a [a]
psiTBST [] = Leaf
psiTBST (x : xs) = Branch (filter (<= x) xs) x (filter (> x) xs)

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST