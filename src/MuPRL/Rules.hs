{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module MuPRL.Rules where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Unbound.Generics.LocallyNameless

import MuPRL.Syntax

data Goal = [(Var, Term)] :>> Term
    deriving (Show)


type MonadRule m = (Fresh m, MonadError RuleError m, MonadReader [(Var, Term)] m)
type Rule m = Goal -> m[Goal]

data RuleError 
    = UnboundIdentifier Var
    | UniverseMismatch Int Int
    | TypeMismatch Term Term
    | RuleMismatch Goal
    deriving (Show)

applyRule :: (MonadRule m) => Term -> Rule m -> m[Goal]
applyRule t rule = join $ asks (rule . flip (:>>) t)

refine :: (MonadRule m) => Term -> (Term -> m (Rule m)) -> m ()
refine t getRule = do
    rule <- getRule t
    goals <- applyRule t rule
    mapM_ (\(ctx :>> t') -> local (const ctx) (refine t' getRule)) goals

lookupHyp :: (MonadError RuleError m) => Var -> [(Var, Term)] -> m Term
lookupHyp v as = case lookup v as of
    Just x -> return x
    Nothing -> throwError $ UnboundIdentifier v

extendHyp :: Var -> Term -> [(Var, Term)] -> [(Var, Term)]
extendHyp v t as = (v,t):as

-- x:A, H |- x = x in A by hyp
hypothesis :: (MonadRule m) => Rule m
hypothesis (ctx :>> Equals (Var x) (Var y) typ) = do
    xtyp <- lookupHyp x ctx
    ytyp <- lookupHyp y ctx
    -- TODO: Equality over types
    if  | not (aeq xtyp ytyp) -> throwError $ TypeMismatch xtyp ytyp
        | not (aeq xtyp typ) -> throwError $ TypeMismatch xtyp typ
        | otherwise -> axiomatic
hypothesis t = ruleMismatch t

-- H |- void in universe k by intro_void
introVoid :: (MonadRule m) => Rule m
introVoid (_ :>> Equals Void Void (Universe _)) = axiomatic
introVoid t = ruleMismatch t

-- H |- unit in universe k by intro_unit
introUnit :: (MonadRule m) => Rule m
introUnit (_ :>> Equals Unit Unit (Universe _))  = axiomatic
introUnit t = ruleMismatch t

introNil :: (MonadRule m) => Rule m
introNil (_ :>> Equals Nil Nil Unit) = axiomatic
introNil  t = ruleMismatch t

-- | universe j = universe i in universe k by intro_universe (max (i,j) < k)
introUniverse :: (MonadRule m) => Rule m
introUniverse (_ :>> Equals (Universe i) (Universe j) (Universe k)) = 
        if (max i j < k)
            then axiomatic
            else throwError $ UniverseMismatch (max i j) k
introUniverse t = ruleMismatch t

introApp :: (MonadRule m) => Term -> Rule m
introApp u@(Pi bnd) (ctx :>> Equals (App f1 a1) (App f2 a2) typ) = do
    ((x, unembed -> atyp), typ') <- unbind bnd
    -- TODO: Unification on `typ` and `typ'`
    let fgoal = (ctx :>> Equals f1 f2 u)
    let agoal = (ctx :>> Equals a1 a2 atyp)
    return [agoal, fgoal]
introApp _ t = ruleMismatch t

introLambda :: (MonadRule m) => Int -> Var -> Rule m
introLambda k v (ctx :>> Equals (Lambda lbnd1) (Lambda lbnd2) (Pi pbnd)) = do
    (x, bx) <- unbind lbnd1
    (y, by) <- unbind lbnd2
    ((_, unembed -> atyp), btyp) <- unbind pbnd
    v' <- fresh v
    -- TODO: Do substitution for dependent types
    let bgoal = (extendHyp v' atyp ctx :>> Equals (subst x (Var v') bx) (subst y (Var v') by) btyp)
    let agoal = (ctx :>> Equals atyp atyp (Universe k))
    return [bgoal, agoal]
introLambda _ _ t = ruleMismatch t

-- Some helper functions
axiomatic :: (Monad m) => m [a]
axiomatic = return []

ruleMismatch :: (MonadError RuleError m) => Goal -> m [a]
ruleMismatch = throwError . RuleMismatch 
