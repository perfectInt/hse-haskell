sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x : xx) (y : yy) (z : zz) = x + y + z : sum3 xx yy zz
sum3 [] (y : yy) (z : zz) = y + z : sum3 [] yy zz
sum3 (x : xx) (y : yy) [] = x + y : sum3 xx yy []
sum3 (x : xx) [] (z : zz) = x + z : sum3 xx [] zz
sum3 [] [] (z : zz) = z : sum3 [] [] zz
sum3 [] (y : yy) [] = y : sum3 [] yy []
sum3 (x : xx) [] [] = x : sum3 xx [] []
sum3 [] [] [] = []

-----------------------------------------------------------------

digits :: Integer -> [Integer]
digits n | n < 0 = digits(-n)
         | n < 10 = [n]
         | otherwise = digits(div n 10) ++ [mod n 10]

-----------------------------------------------------------------

digits' :: Integer -> [Integer]
digits' n | n < 0 = digits'(-n)
         | n < 10 = [n]
         | otherwise = digits'(div n 10) ++ [mod n 10]

containsAllDigits :: Integer -> Bool
containsAllDigits n = all (\d -> elem d digitList) [1..9]
    where digitList = digits n        

-----------------------------------------------------------------

import Data.List (sort)

digits'' :: Integer -> [Integer]
digits'' n | n < 0 = digits''(-n)
         | n == 0 = []
         | otherwise = digits''(div n 10) ++ [mod n 10]
         
containsAllDigits' :: Integer -> Bool
containsAllDigits' n = all (\d -> elem d digitList) [1..9]
    where digitList = filter(/= 0) (digits n)

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = sortedDigits == [1..9] && containsAllDigits n
    where sortedDigits = sort (filter(/= 0) (digits n))

-----------------------------------------------------------------

sublist :: Int -> Int -> [a] -> [a]
sublist n' k' l = take (k - n) (drop n l)
    where
        n = max 0 n'
        k = max 0 k'

-----------------------------------------------------------------

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem _ [] = []
repeatEveryElem n (x : xs) = replicate n x ++ repeatEveryElem n xs

-----------------------------------------------------------------

movingLists :: Int -> [a] -> [[a]]
movingLists n [] = []
movingLists n xs
    | length kusok == n = kusok : movingLists n (tail xs)
    | otherwise = []
  where kusok = take n xs