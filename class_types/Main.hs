{-
	First section
-}

-- 1st
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving (Show, Eq)

elemTree :: Eq a => a -> Tree a -> Bool
elemTree v = bfs . pure
    where
        bfs [] = False
        bfs roots = anyLeaf || bfs (concatMap (\(Node l _ r) -> [l, r]) $ filter (/= Leaf) roots)
            where
                anyLeaf = any ((Just v ==) . toMaybeVal) roots
                toMaybeVal Leaf = Nothing
                toMaybeVal (Node _ x _) = Just x

-- 2nd
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show
  
instance (Eq a) => Eq (Tree a) where
    (/=) Leaf Leaf = False
    (/=) x y = bfsNeq [(x, y)]
        where
            bfsNeq [] = False
            bfsNeq pairs = anyNeq || bfsNeq (concatMap pushDown pairs)
                where
                    anyNeq = any neq pairs
                    neq (Leaf, Leaf) = False
                    neq (Node _ x0 _, Node _ x1 _) = x0 /= x1
                    neq _ = True
                    pushDown (Leaf, Leaf) = []
                    pushDown (Node l0 _ r0, Node l1 _ r1) = [(l0, l1), (r0, r1)]
                    pushDown _ = error "impossible"

-- 3rd
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

-- 4th
data Tree a = Leaf
            | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
  show tree = showTree tree

showTree :: Show a => Tree a -> String
showTree Leaf = "{}"
showTree (Node left x right) = 
  "<" ++ showTree left ++ show x ++ showTree right ++ ">"

-- 5th
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance (Read a) => Read (Tree a) where
  readsPrec _ ('{' : '}' : s) = [(Leaf, s)]
  readsPrec _ ('<' : s) =
    [ (Node l x r, rest)
      | (l, s') <- reads s,
        (x, s'') <- reads s',
        (r, '>' : rest) <- reads s''
    ]
  readsPrec _ _ = []

{-
	Second section
-}

-- 1st
import Data.List (intercalate)

newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix []) = "EMPTY"
    show (Matrix rows) = "[" ++ intercalate "]\n[" (map showRow rows) ++ "]"
      where
        showRow [] = ""
        showRow row = intercalate "," (map show row)

-- 2nd
import Control.Applicative ((<|>))
import Data.Complex (Complex ((:+)), imagPart, realPart)
import Data.Foldable (traverse_)
import GHC.Read (Read (readPrec))
import Text.Read (ReadPrec, get, pfail)

newtype Cmplx = Cmplx (Complex Double) deriving (Eq)

instance Show Cmplx where
  show (Cmplx c)
    | imagPart c < 0 = show (realPart c) ++ "-i*" ++ show (abs $ imagPart c)
    | otherwise = show (realPart c) ++ "+i*" ++ show (abs $ imagPart c)

readChar :: Char -> ReadPrec Char
readChar c = do
  c' <- get
  if c == c' then return c else pfail

instance Read Cmplx where
  readPrec = do
    number <- readPrec
    imagp <- readImagPart
    return $ Cmplx $ number :+ imagp

readImagPart :: ReadPrec Double
readImagPart = do
  c <- readChar '-' <|> readChar '+'
  traverse_ readChar "i*"
  (if c == '-' then negate else id) <$> readPrec

-- 3rd

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
      | x == maxBound = minBound
      | otherwise = succ x

  spred :: a -> a
  spred x
      | x == minBound = maxBound
      | otherwise = pred x

-- 4th
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
  | n >= 0 = helper1 n xs []
  | otherwise = helper1 n' xs []
  where
    n' = (len - (abs n `mod` len)) `mod` len
    len = fromIntegral . length $ xs

helper1 :: Int -> [a] -> [a] -> [a]
helper1 0 xs acc = xs ++ reverse acc
helper1 n xs acc
  | null xs = helper2 n (reverse acc)
  | otherwise = helper1 (pred n) (tail xs) (head xs : acc)

helper2 :: Int -> [a] -> [a]
helper2 _ [] = []
helper2 n xs = suff ++ pref
  where
    (pref, suff) = splitAt n' xs
    len = fromIntegral . length $ xs
    n' = n `mod` len

-- 5th
comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb k (x:xs) = [ x : rest | rest <- comb (k - 1) xs ] ++ comb k xs