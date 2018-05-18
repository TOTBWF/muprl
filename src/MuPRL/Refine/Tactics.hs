module MuPRL.Refine.Tactics where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Core.Term
import MuPRL.Refine.ProofState
import MuPRL.Refine.Rules

data TacticError = RuleError RuleError

newtype Tactic m a = Tactic { unTactic :: a -> m (ProofState a) }

-- | Lifts a rule to the tactic level
rule :: (MonadError TacticError m) => Rule m a -> Tactic m a
rule r = Tactic $ \j -> do
    ss <- runRule j r
    case ss of
        Left e -> throwError $ RuleError e
        Right r -> return r

-- | Identity Tactic
idt :: (Fresh m) => Tactic m Judgement
idt = Tactic $ \j -> wrap j
    
orElse :: (MonadError TacticError m) => Tactic m a -> Tactic m a -> Tactic m a
orElse (Tactic t1) (Tactic t2) = Tactic $ \j -> catchError (t1 j) (const $ t2 j)

try :: (MonadError TacticError m, Fresh m) => Tactic m Judgement -> Tactic m Judgement 
try t = t `orElse` idt

-- -- | Given a list of tactics [t1, ..., tn], create a tactic that when given a proofstate [j1 ... jn], will run ti on ji
-- each :: (Monad m) => [Tactic m Judgement] -> Tactic m (ProofState Judgement)
-- each ts = Tactic $ \(jdgs :#> extract) ok err ->
--     if length ts /= length jdgs then 
