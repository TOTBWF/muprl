{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Function where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Refine.Judgement
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Introduction rule for functions
intro :: (MonadError RuleError m, MonadName m) => TacticT m ()
intro = rule $ \hyp -> \case
    (Pi bnd) -> lunbind bnd $ \(x, a, b) -> do
        body <- subgoal (hyp @> (x, a) |- b)
        wellFormed (hyp |- a)
        return $ lambda x body
    goal -> ruleMismatch "fun/intro" (hyp |- goal)

-- | Non-extensional function equality
eq :: (MonadError RuleError m, MonadName m) => TacticT m ()
eq = rule $ \hyp -> \case
    (Equals (Lam lbnd1) (Lam lbnd2) (Pi pbnd)) ->
      lunbind lbnd1 $ \(x, b1) ->
        lunbind lbnd2 $ \(y, b2) ->
            lunbind pbnd $ \(z, a, b) -> do
                subgoal (hyp @> (z, a) |- (Equals (subst x (Var z) b1) (subst x (Var z) b2) b))
                wellFormed (hyp |- a)
                return Axiom
    goal -> ruleMismatch "fun/eq" (hyp |- goal)

-- | Type equality for functions
eqtype :: (MonadError RuleError m, MonadName m) => TacticT m ()
eqtype = rule $ \hyp -> \case
    (Equals (Pi bnd1) (Pi bnd2) u@(Universe k)) | aeq bnd1 bnd2 -> lunbind bnd1 $ \(x, a, b) -> do
        subgoal (hyp |- (Equals a a u))
        subgoal (hyp @> (x, a) |- (Equals b b u))
        return Axiom
    goal -> ruleMismatch "fun/eqtype" (hyp |- goal)

elim :: (MonadError RuleError m, MonadName m) => Var -> TacticT m ()
elim f = rule $ \hyp g ->
  case Tl.lookupKey f hyp of
    Just p@(Pi _) -> do
      (fapp, ret) <- elimArgs hyp p
      x <- var wildcard
      e <- subgoal (hyp @> (x, ret) |- g)
      return $ subst x fapp e
    Just t -> throwError $ ElimMismatch "fun/elim" t
    Nothing -> throwError $ UndefinedVariable f
  where
    elimArgs :: (MonadError RuleError m, MonadName m) => Telescope Term Term -> Term -> RuleT m (Term, Term)
    elimArgs hyp (Pi bnd) = lunbind bnd $ \(x, a, b) -> do
      a' <- subgoal (hyp |- a)
      (f', ret) <- elimArgs hyp b
      return (App f' a', ret)
    elimArgs _ t = return (Var f, t)
