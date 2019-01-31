{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Equality where

import Control.Monad.Except

-- import Unbound.Generics.LocallyNameless
-- import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName
import MuPRL.Core.Unbound
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule

-- | Kind of a funky introduction rule, looks up bindings in the telescope
intro :: (MonadError RuleError m, MonadName m) => TacticT m ()
intro = rule $ \hyp -> \case
    g@(Equals (Var x) (Var x') t) | x == x' -> 
        case Tl.lookupKey x hyp of
            Just t' | aeq t t' -> return Axiom
            _ -> ruleMismatch "eq/intro" (hyp |- g)
    g -> ruleMismatch "eq/intro" (hyp |- g)
