-- |
-- Module      :  MuPRL.Refine.Tactic
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Tactic
  ( named
  , intro
  , eq
  , eqtype
  , elim
  , assumption
  , T.runTacticT
  ) where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T

import qualified Refinery.Tactic as T

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Core.Telescope (Telescope, (@>))
import qualified MuPRL.Core.Telescope as Tl

import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule
import qualified MuPRL.Refine.Rules.Equality as Equality
import qualified MuPRL.Refine.Rules.Function as Function
import qualified MuPRL.Refine.Rules.Record as Record
import qualified MuPRL.Refine.Rules.Unit as Unit
import qualified MuPRL.Refine.Rules.Universe as Universe
import qualified MuPRL.Refine.Rules.Void as Void

match :: (MonadName m) => (Telescope Term Term -> Term -> TacticT m ()) -> TacticT m ()
match f = T.match $ \j -> lunbind j (uncurry f)

named :: (MonadError RuleError m, MonadName m) => Text -> TacticT m ()
named = \case
  "eq/intro" -> Equality.intro
  "fun/intro" -> Function.intro
  "fun/eq" -> Function.eq
  "fun/eqtype" -> Function.eqtype
  "universe/eqtype" -> Universe.eqtype
  "void/eqtype" -> Void.eqtype
  t -> throwError $ NoSuchRule t

intro :: (MonadError RuleError m, MonadName m) => TacticT m ()
intro = match $ \hyp -> \case
  (Pi _) -> Function.intro
  (Equals (Var x) (Var x') _) -> Equality.intro
  goal -> ruleMismatch "intro" (hyp |- goal)

eq :: (MonadError RuleError m, MonadName m) => TacticT m ()
eq = match $ \hyp -> \case
  (Equals (Lam _) (Lam _) (Pi _)) -> Function.eq
  goal -> ruleMismatch "eq" (hyp |- goal)

eqtype :: (MonadError RuleError m, MonadName m) => TacticT m ()
eqtype = match $ \hyp -> \case
  (Equals (Pi _) (Pi _) (Universe _)) -> Function.eqtype
  (Equals (Universe _) (Universe _) (Universe _)) -> Universe.eqtype
  (Equals Void Void (Universe _)) -> Void.eqtype
  goal -> ruleMismatch "eqtype" (hyp |- goal)

elim :: (MonadError RuleError m, MonadName m) => Text -> TacticT m ()
elim x = match $ \hyp _ ->
  let x' = string2Name $ T.unpack x
  in case Tl.lookupKey x' hyp of
    (Just (Pi _)) -> Function.elim x'
    (Just goal) -> throwError $ ElimMismatch "elim" goal
    Nothing -> throwError $ UndefinedVariable (s2n $ T.unpack x)

assumption :: (MonadError RuleError m, MonadName m) => TacticT m ()
assumption = rule $ \hyp goal ->
  case (Tl.find (aeq goal) hyp) of
    Just (x,_) -> return $ Var x
    Nothing -> throwError $ NotInContext goal
