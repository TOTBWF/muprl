module MuPRL.Vernacular.Eval where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.PrettyPrint
import MuPRL.Error

import MuPRL.Core.Term
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import MuPRL.Refine.Tactic

import MuPRL.Vernacular.Syntax

data VernacularError
    = TacticError TacticError
    | UnprovedSubgoals [Judgement]

instance Error VernacularError where
    errorText (TacticError e) = errorText e
    errorText (UnprovedSubgoals js) = pretty "Unproved Subgoals: " <+> vcat (fmap pretty js)

evalVernacular :: (Fresh m) => Vernacular m -> m (Either VernacularError Term)
evalVernacular (Theorem _ goal tac) = runExceptT $ do
    r <- lift $ runTactic (Tl.empty |- goal) tac
    case r of
        Left e -> throwError $ TacticError e
        Right (ProofState bnd) -> do
            (subgoals, extract) <- unbind bnd
            if Tl.null subgoals
                then return extract
                else throwError $ UnprovedSubgoals (fmap snd $ Tl.toList subgoals)