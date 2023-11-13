{-
    First section
-}

-- 1st
doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = do
  bd <- next ini
  rest <- doNTurns (n - 1) bd
  return rest

{-
    Second section
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

-- 5th
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) (Un c) = Bi a b (Un c)
concat3OC (Un a) (Un b) (Bi c1 c2 cs) = Bi a b (Bi c1 c2 cs)
concat3OC (Un a) (Bi b1 b2 bs) c = Bi a b1 (concat3OC (Un b2) bs c)
concat3OC (Bi a1 a2 as) b c = Bi a1 a2 (concat3OC as b c)
