module MuPRL.Vernacular.Eval where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.PrettyPrint
import MuPRL.Error

import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule
import MuPRL.Refine.Tactic

import MuPRL.Vernacular.Syntax

data VernacularError
    = TacticError RuleError
    | UnprovedSubgoals [Judgement] Term

instance Error VernacularError where
    errorText (TacticError e) = errorText e
    errorText (UnprovedSubgoals js e) =
        let pInfo = pretty "Unproved Subgoals:" <+> (align $ vsep $ fmap pretty js)
            pDbg = align $ vsep $
                [ pretty "[Debug Info]"
                , pretty "Extract:" <+> pretty e
                -- , pretty "Proof State (Raw):" <+> (pretty $ show e)
                ]
        in (align $ vsep [pInfo, pDbg])

evalVernacular :: (Monad m) => Vernacular m -> m (Either VernacularError Term)
evalVernacular (Theorem _ goal tac) =
  (runExceptT $ runNameMT $ runTacticT tac (Tl.empty |- goal)) >>= \case
    Left e -> return $ Left $ TacticError e
    Right (e, sg) -> if null sg then return $ Right e else return $ Left $ UnprovedSubgoals sg e


    -- r <- lift $ runTactic (Tl.empty |- goal) tac
    -- case r of
        -- Left e -> throwError $ TacticError e
        -- Right p@(ProofState bnd) -> lunbind bnd $ \(subgoals, extract) ->
        --     if Tl.null subgoals
        --         then return extract
        --         else throwError $ UnprovedSubgoals (snd <$> Tl.toList subgoals) p
