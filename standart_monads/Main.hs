{-
    Classroom section
-}

-- 1st
import Control.Monad

data Logged a = Logged String a deriving (Eq, Show)

instance Functor Logged where
  fmap = liftM

instance Applicative Logged where
  pure = return
  (<*>) = ap

instance Monad Logged where
  return = Logged ""
  (>>=) (Logged s x) f = Logged (s' ++ s) y
    where
      Logged s' y = f x

write2log :: String -> Logged ()
write2log s = Logged s ()


logIt :: Show b => b -> Logged b
logIt v = do
  write2log $ "var = " ++ show v ++ "; "
  return v

{-
    Hw section
-}

-- 1st
import Control.Monad.Writer

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR x xs | null xs = writer (x, show x)
                  | otherwise = do
                        let res = head xs - fst (runWriter $ minusLoggedR x (tail xs))
                        let a = execWriter (listen (minusLoggedR x (tail xs)))
                        tell ("(" ++ show (head xs) ++ "-" ++ a ++ ")")
                        return res

-- 2nd
import Control.Monad.Writer

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL x = go (writer (x, show x))
  where
    go e [] = e
    go e es = go (writer (ans - head es, "(" ++ a ++ "-" ++ show (head es) ++ ")")) $ tail es
      where
        ans = fst $ runWriter e
        a = execWriter (listen e)

-- 3rd
import Control.Monad.State

fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0, 1)

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a, b) <- get
    put (b, a + b)


-- 4th
import Data.IORef

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  val <- readIORef ref
  if p val
    then do
      action
      modifyIORef' ref (\v -> v)
      while ref p action
    else return ()

-- 5th
import Control.Monad
import System.Random

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  l <- replicateM k $ iter n
  return $ sum l / fromIntegral k
  where
    iter i = do
      l <- replicateM i flipCoin
      return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

flipCoin :: IO Int
flipCoin = randomRIO (0, 1) :: IO Int

-- 6th
import Control.Monad
import System.Random
import Control.Monad.State

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x,y) = do
    gen <- get
    let (a, gen') = randomR (x, y) gen
    put gen'
    return a

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  l <- replicateM k $ iter n
  return $ sum l / fromIntegral k
  where
    iter i = do
      l <- replicateM i flipCoin
      return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

flipCoin :: State StdGen Int      
flipCoin = randomRState (0::Int, 1::Int)

-- 7th
import System.Random
import Control.Monad
import Control.Monad.State

gen :: StdGen
gen = mkStdGen 622

flipCoin :: [Int]
flipCoin = randomRs (0, 1) gen

avgdev'' :: Int -> Int -> Double
avgdev'' k n = helper k n flipCoin 0 n / fromIntegral k

helper :: Int -> Int -> [Int] -> Int -> Int -> Double
helper 0 _ _ _ _ = 0
helper _ _ [] _ _ = 0
helper k 0 (_ : xs) summ n' = abs (fromIntegral n' / 2 - fromIntegral summ) + helper (k - 1) n' xs 0 n'
helper k n (x : xs) summ n' = helper k (n - 1) xs (x + summ) n'