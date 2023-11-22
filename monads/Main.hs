{-
    Classroom section
-}

-- 1st
doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = do
  bd <- next ini
  rest <- doNTurns (n - 1) bd
  return rest

{-
    Hw section
-}

-- 1st
surround :: a -> a -> [a] -> [a]
surround x y zs = do
  z <- zs
  [x, z, y]

-- 2nd
lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups x ys = do
  (key, value) <- ys
  if key == x then
    return value
  else
    []

-- 3rd
factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  a <- filter (\x -> mod n x == 0) [1 .. round (sqrt (fromIntegral n))]
  return (a, n `div` a)

-- 4th
absDiff :: Num a => [a] -> [a]
absDiff xs = do 
    a <- zipWith (-) as bs
    return (abs a)
    where as | null xs = []
             | otherwise = init xs
          bs | null xs = []
             | otherwise = tail xs

-- 5th
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) (Un c) = Bi a b (Un c)
concat3OC (Un a) (Un b) (Bi c1 c2 cs) = Bi a b (Bi c1 c2 cs)
concat3OC (Un a) (Bi b1 b2 bs) c = Bi a b1 (concat3OC (Un b2) bs c)
concat3OC (Bi a1 a2 as) b c = Bi a1 a2 (concat3OC as b c)

-- 6th
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) t) = Bi x y $ concatOC t
concatOC (Bi (Bi x1 x2 x3) y t) = Bi x1 x2 $ concatOC (Bi x3 y t)
concatOC (Bi (Un x) (Bi y1 y2 y3) t) = Bi x y1 $ concatOC (Bi (Un y2) y3 t)

-- 7th
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) t) = Bi x y $ concatOC t
concatOC (Bi (Bi x1 x2 x3) y t) = Bi x1 x2 $ concatOC (Bi x3 y t)
concatOC (Bi (Un x) (Bi y1 y2 y3) t) = Bi x y1 $ concatOC (Bi (Un y2) y3 t)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) t = Bi x y t
concat3OC (Bi x1 x2 x3) y t = Bi x1 x2 $ concat3OC x3 y t
concat3OC (Un x) (Bi y1 y2 y3) t = Bi x y1 $ concat3OC (Un y2) y3 t

instance Functor OddC where
  fmap f (Un x) = Un $ f x
  fmap f (Bi x y t) = Bi (f x) (f y) (f <$> t)

instance Applicative OddC where
  pure = Un

  (Un f) <*> (Un x) = Un $ f x
  (Un f) <*> (Bi x1 x2 x3) = Bi (f x1) (f x2) (f <$> x3)
  (Bi f1 f2 f3) <*> (Un x) = Bi (f1 x) (f2 x) (f3 <*> Un x)
  (Bi f1 f2 f3) <*> (Bi x1 x2 x3) = concat3OC a b c
    where
      a = Bi (f1 x1) (f1 x2) (f1 <$> x3)
      b = Bi (f2 x1) (f2 x2) (f2 <$> x3)
      c = concatOC $ Bi (f3 <*> Un x1) (f3 <*> Un x2) (Un $ f3 <*> x3)

instance Monad OddC where
  (Un x) >>= f = f x
  (Bi x y t) >>= f = concat3OC a b c
    where
      a = f x
      b = f y
      c = t >>= f