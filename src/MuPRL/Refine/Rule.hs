-- |
-- Module      :  MuPRL.Refine.Rule
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE ConstraintKinds #-}
module MuPRL.Refine.Rule
  ( TacticT
  , RuleT
  , MonadRule
  , subgoal
  , rule
  , RuleError(..)
  , ruleMismatch
  , wellFormed
  ) where

import Control.Monad.Except

import Data.Text (Text)

import Refinery.Tactic (subgoal)
import qualified Refinery.Tactic as T

import MuPRL.Error
import MuPRL.PrettyPrint
import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName
import MuPRL.Core.Telescope
import MuPRL.Refine.Judgement

type TacticT m = T.TacticT Judgement Term m
type RuleT m = T.RuleT Judgement Term m
type MonadRule m = T.MonadRule Judgement Term m

rule :: (MonadName m) => (Telescope Term Term -> Term -> RuleT m Term) -> TacticT m ()
rule r = T.rule $ \j -> lunbind j (uncurry r)

data RuleError
    = UniverseMismatch Int Int
    | TypeMismatch Term Term
    | NotInContext Term
    | UndefinedVariable Var
    | RuleMismatch Text Judgement
    | GoalMismatch Text Term
    | ElimMismatch Text Term
    | NoSuchRule Text

instance Error RuleError where
    errorText (UniverseMismatch i j) = pretty "Universe Mismatch:" <+> pretty i <+> pretty "and" <+> pretty j
    errorText (TypeMismatch t1 t2) = pretty "Type Mismatch:" <+> pretty t1 <+> pretty "and" <+> pretty t2
    errorText (UndefinedVariable x) = pretty "Undefined Variable:" <+> pretty x <+> (pretty $ show x)
    errorText (NotInContext t) = pretty "Not In Context:" <+> pretty t
    errorText (RuleMismatch t j) = pretty "Rule Mismatch:" <+> vsep [pretty t <+> pretty j, pretty $ show j]
    errorText (GoalMismatch t j) = pretty "Goal Mismatch:" <+> pretty t <+> pretty j
    errorText (ElimMismatch t j) = pretty "Elim Mismatch:" <+> pretty t <+> pretty j
    errorText (NoSuchRule t) = pretty "No Such Rule:" <+> pretty t


ruleMismatch :: (MonadError RuleError m) => Text -> Judgement -> m a
ruleMismatch t jdg = throwError $ RuleMismatch t jdg

-- | Infers the universe level of a given term
inferUniverse :: (MonadName m) => Term -> m Int
inferUniverse (Universe k) = return $ k + 1
inferUniverse (Pi bnd) = lunbind bnd $ \(_, a, b) -> max <$> inferUniverse a <*> inferUniverse b
inferUniverse (Equals _ _ a) = inferUniverse a
inferUniverse _ = return 0

-- | Create a well-formedness goal
wellFormed :: (MonadRule m, MonadName m) => Judgement -> m ()
wellFormed j = lunbind j $ \(hy, goal) -> do
    k <- inferUniverse goal
    _ <- subgoal (hy |- Equals goal goal (Universe k))
    return ()
