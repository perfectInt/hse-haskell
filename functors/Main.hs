{-
    First section
-}

-- 1st
{-# LANGUAGE InstanceSigs #-}

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap _ (L l) = L l
  fmap f (R a) = R $ f a

instance Applicative (E l) where
  pure :: a -> E l a
  pure  = R
  (<*>) :: E l (a -> b) -> E l a -> E l b
  L l <*> _ = L l
  _ <*> L l = L l
  R ab <*> R a = R $ ab a

-- 2nd
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
  fmap f (Cmps c) = Cmps $ fmap (fmap f) c

{-
    Second section
-}

-- 1st
import Control.Applicative

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList $ f <$> ZipList xs

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList $ ZipList fs <*> ZipList xs

-- 2nd
data Triple a = Tr a a a  deriving (Eq, Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure x = Tr x x x
    Tr f1 f2 f3 <*> Tr x1 x2 x3 = Tr (f1 x1) (f2 x2) (f3 x3)

-- 3rd
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch left x right) = Branch (fmap f left) (f x) (fmap f right)

instance Applicative Tree where
    pure x = Branch (pure x) x (pure x)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Branch leftF f rightF) <*> (Branch left x right) =
        Branch (leftF <*> left) (f x) (rightF <*> right)

-- 4th
newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap h (Cmps f) = Cmps $ (fmap . fmap) h f

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure x = Cmps (pure (pure x))
    (Cmps fgx1) <*> (Cmps fgx2) = Cmps ((<*>) <$> fgx1 <*> fgx2)

-- 5th
import Control.Applicative

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' [] = ("1.0", 1.0)
divideList' (x:xs) = (logString, result)
  where
    (logString', previousResult) = divideList' xs
    result = x / previousResult
    logString = "<-" ++ show x ++ "/" ++ logString'

-- 6th
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
    fmap f (Arr2 g) = Arr2 (\e1 e2 -> f (g e1 e2))

instance Functor (Arr3 e1 e2 e3) where
    fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f (g e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
    pure x = Arr2 (\_ _ -> x)
    (Arr2 f) <*> (Arr2 g) = Arr2 (\e1 e2 -> f e1 e2 (g e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
    pure x = Arr3 (\_ _ _ -> x)
    (Arr3 f) <*> (Arr3 g) = Arr3 (\e1 e2 e3 -> f e1 e2 e3 (g e1 e2 e3))
