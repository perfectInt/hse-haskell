{-
    First section
-}

-- 1st
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (fmapDefault, foldMapDefault)

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap = fmapDefault

instance Foldable Result where
  foldMap :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap = foldMapDefault

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error e) = pure $ Error e
  traverse f (Ok a) = Ok <$> f a

-- 2nd
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (foldMapDefault)

data NEList a
  = Single a
  | Cons a (NEList a)
  deriving (Eq, Show)

instance Functor NEList where
  fmap :: (a -> b) -> NEList a -> NEList b
  fmap f (Single v) = Single $ f v
  fmap f (Cons a rest) = Cons (f a) $ f <$> rest

instance Foldable NEList where
  foldMap :: (Monoid m) => (a -> m) -> NEList a -> m
  foldMap = foldMapDefault

instance Traversable NEList where
  sequenceA :: (Applicative f) => NEList (f a) -> f (NEList a)
  sequenceA (Single v) = Single <$> v
  sequenceA (Cons a rest) = Cons <$> a <*> sequenceA rest

-- 3rd
nat :: Parser Char Int
nat = read . concatMap show <$> some digit

{-
    Second section
-}

-- 2nd
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch left x right) = Branch (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
    foldMap _ Nil = mempty
    foldMap f (Branch left x right) = foldMap f left <> f x <> foldMap f right

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch left x right) = Branch <$> traverse f left <*> f x <*> traverse f right

-- 3rd
import Data.Foldable (Foldable, foldMap)

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
    foldMap h (Cmps fgx) = foldMap (foldMap h) fgx

-- 5th
import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> [(f x, rest) | (x, rest) <- p input]

instance Applicative Parser where
    pure x = Parser $ \input -> [(x, input)]
    (Parser pf) <*> (Parser px) = Parser $ \input -> [(f x, rest) | (f, rest1) <- pf input, (x, rest) <- px rest1]

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input ++ p2 input

-- 6th
class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
  unit = Just ()
  Just a  *&* Just b  = Just (a, b)
  _       *&* _       = Nothing

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (s1, a) *&* (s2, b) = (s1 <> s2, (a, b))

instance Monoidal ((->) e) where
  unit = const ()
  f *&* g = \e -> (f e, g e)

-- 7th
unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a, b)
pair' fa fb = (,) <$> fa <*> fb
