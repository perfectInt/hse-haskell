-- 1st

{-# LANGUAGE FlexibleContexts #-}

import Data.List ( nub, (\\), union )

infixl 4 :@

infixr 3 :->

type Symb = String

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

freeVars :: Expr -> [Symb]
freeVars (Var name) = [name]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam name body) = freeVars body \\ [name]

freeTVars :: Type -> [Symb]
freeTVars (TVar name) = [name]
freeTVars (s :-> t) = freeTVars s `union` freeTVars t

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) name ttype = Env (env ++ [(name, ttype)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (\(_, t) -> freeTVars t) env

-- 2nd

{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
    Just t  -> return t
    Nothing -> throwError $ "There is no variable \"" ++ v ++ "\" in the environment."

-- 3rd
import qualified Data.Bifunctor
import Control.Arrow (Arrow(second))
infixl 4 :@

infixr 3 :->

type Symb = String

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (second (appSubsTy sbs)) env

-- 4th

import Data.List (nub, union, (\\))

infixl 4 :@

infixr 3 :->

type Symb = String

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) =
  let carrier = map fst
      result = carrier s1 `union` carrier s2
   in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

-- 5th

{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (Arrow (second))
import Control.Monad.Except
import Data.List (nub, union, (\\))

infixl 4 :@

infixr 3 :->

type Symb = String

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

freeVars :: Expr -> [Symb]
freeVars (Var name) = [name]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam name body) = freeVars body \\ [name]

freeTVars :: Type -> [Symb]
freeTVars (TVar name) = [name]
freeTVars (s :-> t) = freeTVars s `union` freeTVars t

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) name ttype = Env (env ++ [(name, ttype)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (\(_, t) -> freeTVars t) env

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
  Just ttype -> pure ttype
  Nothing -> throwError $ "There is no variable " <> show v <> " in the enviroment."

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (second (appSubsTy sbs)) env

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) =
  let carrier = map fst
      result = carrier s1 `union` carrier s2
   in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []
 
unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar a) (TVar b)
  | a == b = return $ SubsTy []
unify (TVar a) t
  | a `elem` freeTVars t = throwError $ "Can't unify  (" ++ show a ++ ") with (" ++ show t ++ ")!"
  | otherwise = return $ SubsTy [(a, t)]
unify (s1 :-> s2) (TVar a) = unify (TVar a) (s1 :-> s2)
unify (s1 :-> s2) (t1 :-> t2) = do
  let h = unify s2 t2
  case h of
    Left err -> throwError err
    Right s -> do
      let h' = unify (appSubsTy s s1) (appSubsTy s t1)
      case h' of
        Left err -> throwError err
        Right s' -> return $ composeSubsTy s' s

-- 6th

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow (Arrow (second))
import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char
import Data.List
import Data.Monoid
import Data.Unique

infixl 4 :@

infixr 3 :->

type Symb = String

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

freeVars :: Expr -> [Symb]
freeVars (Var name) = [name]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam name body) = freeVars body \\ [name]

freeTVars :: Type -> [Symb]
freeTVars (TVar name) = [name]
freeTVars (s :-> t) = freeTVars s `union` freeTVars t

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) name ttype = Env (env ++ [(name, ttype)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (\(_, t) -> freeTVars t) env

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
  Just ttype -> pure ttype
  Nothing -> throwError $ "There is no variable " <> show v <> " in the enviroment."

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (second (appSubsTy sbs)) env

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) =
  let carrier = map fst
      result = carrier s1 `union` carrier s2
   in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar a) (TVar b)
  | a == b = return $ SubsTy []
unify (TVar a) t
  | a `elem` freeTVars t = throwError $ "Can't unify  (" ++ show a ++ ") with (" ++ show t ++ ")!"
  | otherwise = return $ SubsTy [(a, t)]
unify (s1 :-> s2) (TVar a) = unify (TVar a) (s1 :-> s2)
unify (s1 :-> s2) (t1 :-> t2) = do
  let h = unify s2 t2
  case h of
    Left err -> throwError err
    Right s -> do
      let h' = unify (appSubsTy s s1) (appSubsTy s t1)
      case h' of
        Left err -> throwError err
        Right s' -> return $ composeSubsTy s' s

stepTVar :: MonadState Integer m => m Type
stepTVar = do
  n <- get
  modify (+ 1)
  return $ TVar $ show n

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type, Type)]
equations env' expr' t' = evalStateT (equations' env' expr' t') 1
  where
    equations' :: MonadError String m => Env -> Expr -> Type -> StateT Integer m [(Type, Type)]
    equations' env (Var x) t = do
      t'' <- appEnv env x
      return [(t, t'')]
    equations' env (m :@ n) t = do
      a <- stepTVar
      e <- equations' env m (a :-> t)
      e' <- equations' env n a
      return $ e `union` e'
    equations' env (Lam v m) t = do
      a <- stepTVar
      b <- stepTVar
      e <- equations' (extendEnv env v a) m b
      return $ e `union` [(a :-> b, t)]

principlePair :: MonadError String m => Expr -> m (Env, Type)
principlePair expr =
  let e = Env $ makeEnv (-1 ::Int) $ freeVars expr
      s = TVar "_"
   in do
        syst <- equations e expr s
        let (a1, a2) = helper syst
        sbs <- unify a1 a2
        return (appSubsEnv sbs e, appSubsTy sbs s)
  where
    helper = foldr1 (\(x, x') (y, y') -> (x :-> y, x' :-> y'))
    makeEnv _ [] = []
    makeEnv i (x : xs) = (x, TVar $ show i) : makeEnv (i - 1) xs