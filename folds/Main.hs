{-
    First section
-}

-- 1st
drop' :: Int -> [a] -> [a]
drop' n xs =
	foldr step ini xs n

step a g = \n -> if n <= 0 then a : g 0 else g (n - 1) 
ini _ = []

{-
    Second section
-}

-- 1st
import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun :: (Char, Char) -> Maybe (Char, (Char, Char))
fun (a, b)
  | b < a = Nothing
  | otherwise = Just (b, (a, pred b))

-- 2nd
tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun x [] = [x] : []
fun x (y:ys) = (x:y) : (y:ys)
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' x [] = [] : []
fun' x (y:ys) = [] : map (x:) (y:ys)
ini' = [[]]

-- 3rd
reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini'

fun' :: a -> [a] -> [a]
fun' x xs = xs ++ [x]

ini' :: [a]
ini' = []

fun'' :: [a] -> a -> [a]
fun'' xs x = x : xs

ini'' :: [a]
ini'' = []

-- 4th
infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n
fun _ _ m | m < 0 = Nothing
fun x _ 0 = Just x
fun _ r m = r (m - 1)
ini _ = Nothing


-- 5th
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
fun :: (b -> a -> b) -> a -> (b -> b) -> (b -> b)
fun g x h = \b -> h (g b x)
    
ini :: b -> b
ini = id

-- 6th
-- А шестую не получите, потому что ее делал не я, и я пообещал, что никому ее не раскрою