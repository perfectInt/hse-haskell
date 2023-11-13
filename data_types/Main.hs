{-
    First section
-}

-- 1st
type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

arity (TVar _) = 0
arity (_ :-> next) = 1 + arity next

order (TVar _) = 0
order (a :-> b) = max(order a + 1) (order b)

-- 2nd
import Data.List (nub)

type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

infixl 4 :@
data Term = Var Symb
          | Term :@ Term
          | Lam Symb Type Term
    deriving (Eq,Show)

freeVars :: Term -> [Symb]
freeVars (Var x) = [x]
freeVars (Lam x _ t) = filter (/= x) (freeVars t)
freeVars (t1 :@ t2) = nub (freeVars t1 ++ freeVars t2)

boundVars :: Term -> [Symb]
boundVars (Var _) = []
boundVars (Lam x _ t) = x : boundVars t
boundVars (t1 :@ t2) = boundVars t1 ++ boundVars t2

-- 3rd
import Data.List (find)

type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

infixl 4 :@
data Term = Var Symb
          | Term :@ Term
          | Lam Symb Type Term
    deriving (Eq,Show)

type Env = [(Symb,Type)]

infer0 :: Term -> Maybe Type
infer0 = infer [] 

infer :: Env -> Term -> Maybe Type
infer env (Var x) = lookup x env
infer env (Lam x t1 t2) = do
  t2' <- infer ((x, t1) : env) t2
  return (t1 :-> t2')
infer env (t1 :@ t2) = do
  t1' <- infer env t1
  t2' <- infer env t2
  case t1' of
    t11 :-> t12 | t11 == t2' -> return t12
    _ -> Nothing

{-
    Second section
-}

-- 1st
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (rank x) (rank y)
	where
		rank Error = 3
		rank Warning = 2
		rank Info = 1

-- 2nd
data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p@Person { firstName = name }
	| length name < 2 = p
	| otherwise = p { firstName = take 1 name ++ "." }

-- 3rd
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left value right) = value + treeSum left + treeSum right

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left _ right) = 1 + max (treeHeight left) (treeHeight right)