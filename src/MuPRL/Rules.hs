{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module MuPRL.Rules where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Unbound.Generics.LocallyNameless
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import Data.Foldable (find)

import MuPRL.Syntax
import MuPRL.LCF
import MuPRL.Environment

data RuleError 
    = UniverseMismatch Int Int
    | TypeMismatch Term Term
    | UnboundIdentifier Var
    | RuleMismatch Judgement
    deriving (Show)

type MonadRule m = (MonadError RuleError m, Fresh m)

intro :: (MonadRule m) => Rule m
intro = Rule $ \case
    -- Unit
    (_ :>> Unit) -> return (Seq.empty :#> Nil)
    -- Equality rules
    (_ :>> Equals Void Void (Universe _)) -> axiomatic
    (_ :>> Equals Unit Unit (Universe _)) -> axiomatic
    (_ :>> Equals Nil Nil Unit) -> axiomatic
    (h :>> Equals (Lambda lbnd1) (Lambda lbnd2) (Pi pbnd)) -> do
        (x, bx) <- unbind lbnd1
        (y, by) <- unbind lbnd2
        ((z, unembed -> atyp), btyp) <- unbind pbnd
        v' <- fresh x
        k <- inferUniverse atyp
        (aGoal, _) <- goal (Seq.empty :>> Equals atyp atyp (Universe k))
        (bGoal, _) <- goal ((h :|> (v', atyp)) :>> Equals (subst x (Var v') bx) (subst y (Var v') by) (subst z (Var v') btyp))
        return ((Seq.empty :|> aGoal :|> bGoal) :#> Axiom)
    (_ :>> Equals (Universe i) (Universe j) (Universe k)) ->
        if (max i j < k) then axiomatic
        else throwError $ UniverseMismatch (max i j) k
    jdg -> ruleMismatch jdg

lookupHyp :: (MonadError RuleError m) => Var -> Seq (Var, Term) -> m Term
lookupHyp v as = case find ((==) v . fst) as of
    Just (_, t) -> return t
    Nothing -> throwError $ UnboundIdentifier v

hypothesis :: (MonadRule m) => Rule m
hypothesis = Rule $ \case
    (ctx :>> Equals (Var x) (Var y) typ) -> do
        xtyp <- lookupHyp x ctx
        ytyp <- lookupHyp y ctx
        -- TODO: Equality over types
        if  | not (aeq xtyp ytyp) -> throwError $ TypeMismatch xtyp ytyp
            | not (aeq xtyp typ) -> throwError $ TypeMismatch xtyp typ
            | otherwise -> axiomatic
    jdg -> ruleMismatch jdg
        
inferUniverse :: (MonadRule m) => Term -> m Int
inferUniverse (Universe k) = return $ k + 1
inferUniverse (Pi bnd) = do
    ((_, unembed -> atyp), btyp) <- unbind bnd
    max <$> inferUniverse atyp <*> inferUniverse btyp
inferUniverse (Equals _ _ a) = inferUniverse a
inferUniverse _ = return 0

introApp :: (MonadRule m) => Term -> Rule m
introApp u@(Pi bnd) = Rule $ \case 
    (ctx :>> Equals (App f1 a1) (App f2 a2) typ) -> do
        ((x, unembed -> atyp), typ') <- unbind bnd
        -- TODO: Unification on `typ` and `typ'`
        (fgoal, _) <- goal (ctx :>> Equals f1 f2 u)
        (agoal, _) <- goal (ctx :>> Equals a1 a2 atyp)
        return ((Seq.empty :|> agoal :|> fgoal) :#> Axiom)
    jdg -> ruleMismatch jdg

ruleMismatch :: (MonadError RuleError m) => Judgement -> m ProofState
ruleMismatch = throwError . RuleMismatch 
