-- Реализуйте алгоритм вывода типов.
-- (10 баллов)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeInference where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Monoid
import Data.List

type TermSymb = String

infix 2 :@

data Term
        = Var TermSymb
        | Term :@ Term
        | Lam TermSymb Term
        deriving (Eq, Show)

type TypeSymb = String

infix 3 :->

data Type
        = TVar TypeSymb
        | Type :-> Type
        deriving (Eq, Show)

newtype Env = Env [(TermSymb, Type)]

newtype Subst = Subst [(TypeSymb, Type)]

($#) = applySubst
infix 3 $#

($*) = composeSubst
infix 2 $*

hasTSymb :: TypeSymb -> Type -> Bool
hasTSymb tsymb (t1 :-> t2) = (hasTSymb tsymb t1) || (hasTSymb tsymb t2)
hasTSymb tsymb (TVar ts) = tsymb == ts

applySubst :: Subst -> Type -> Type
applySubst (Subst lst) t@(TVar tsymb) = case lookup tsymb lst of Nothing -> t
                                                                 Just nt -> nt
applySubst subst (t1 :-> t2) = (applySubst subst t1) :-> (applySubst subst t2)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1@(Subst l1) s2@(Subst l2) = Subst $ zip tsymbs types
    where
        tsymbs = nub $ (fst $ unzip l1) ++ (fst $ unzip l2)
        types = map ((applySubst s1) . (applySubst s2) . TVar) tsymbs

instance Monoid Subst where
    mempty = Subst []
    mappend = composeSubst

addToEnv :: (TermSymb, Type) -> Env -> Env
addToEnv tt (Env lst) = Env $ tt : lst

getType :: (MonadError String m,
            MonadReader Env m) => TermSymb -> m Type
getType symb = do
    Env context <- ask
    case lookup symb context of Nothing -> throwError ("not in context " ++ symb)
                                Just t  -> return t

getFreshTVar :: (MonadState Int m) => m Type
getFreshTVar = do
    i <- get
    modify (+1)
    return $ TVar $ "t" ++ show i

equations :: (MonadError String m,
              MonadReader Env m,
              MonadState Int m) => Term -> Type -> m [(Type, Type)]
equations (Var x) sigma = do
    xt <- getType x
    return [(xt, sigma)]

equations (x :@ y) sigma = do
    alpha <- getFreshTVar
    xeq <- equations x (alpha :-> sigma)
    yeq <- equations y alpha
    return $ xeq ++ yeq

equations (Lam x y) sigma = do
   alpha <- getFreshTVar
   beta <- getFreshTVar
   yeq <- local (addToEnv (x, alpha)) $ equations y beta
   return $ (alpha :-> beta, sigma) : yeq

unify :: (MonadError String m) => Type -> Type -> m Subst
unify t1@(TVar _) t2@(TVar _) | t1 == t2  = return $ Subst []
unify (TVar ts) t = if hasTSymb ts t then throwError "no solutions"
                                     else return $ Subst [(ts, t)]
unify t1@(_ :-> _) t2@(TVar _) = unify t2 t1
unify (s1 :-> s2) (t1 :-> t2) = do
    u2 <- unify s2 t2
    u1 <- unify (u2 $# s1) (u2 $# t1)
    return $ u1 $* u2

unifyEqs :: (MonadError String m) => [(Type, Type)] -> m Subst
unifyEqs eqs = let (lt, rt) =  unzip eqs in unify (foldl1 (:->) lt) (foldl1 (:->) rt)

inferType :: Term -> Either String Type
inferType term = do
    eqs <- runExcept $ runReaderT (evalStateT (equations term (TVar "t0")) 1) (Env [])
    subst <- unifyEqs eqs
    return $ subst $# (TVar "t0")
