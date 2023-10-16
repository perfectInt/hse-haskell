{-
1. snd
2. uncurry, flip, (,)
-}

doubleFact :: Integer -> Integer
doubleFact n = if n <= 1 then 1 else n * doubleFact(n-2)

-----------------------------------------------------------------

seqBs :: [Integer]
seqBs = map seqB' [0..]

seqB' :: Integer -> Integer
seqB' 0 = 1
seqB' 1 = 2
seqB' 2 = 3
seqB' n = seqB (n - 1) - 2 * seqB (n - 2) + 3 * seqB (n - 3)

seqB :: Integer -> Integer
seqB n = seqBs !! (fromInteger n)

-----------------------------------------------------------------

fibonacci :: Integer -> Integer
fibonacci n
    | n > 1 = helper n 1 0 1
    | n < 0 = if even n then (-(helper (-n) 1 0 1)) else helper (-n) 1 0 1
    | True = n
    
helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n curIdx first second
    | curIdx == n = second
    | True = helper n (curIdx + 1) second (first + second)

-----------------------------------------------------------------

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n | n == 0 = (0,1)
              | n < 0  = sum'n'count (-n)
              | otherwise = h n 0 0 where
                 h n s k | n == 0 = (s,k)
                         | otherwise = h (div n 10) (s + mod n 10) (k+1)

-----------------------------------------------------------------

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + (sum $ map f xs))
    where
        n = 1000
        h = (b - a) / n
        xs = map(\x -> a + h * x) [1..n-1]