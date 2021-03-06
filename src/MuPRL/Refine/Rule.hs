{-# LANGUAGE ConstraintKinds #-}
module MuPRL.Refine.Rule where

import Control.Monad.Except
import Control.Monad.Writer

import Data.Text (Text)

import MuPRL.Error
import MuPRL.PrettyPrint
import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))

import Debug.Trace

type MonadRule m = (MonadName m)

newtype Rule m a = Rule { unRule :: a -> ExceptT RuleError m (ProofState a) }

runRule :: a -> Rule m a -> m (Either RuleError (ProofState a))
runRule a (Rule r) = runExceptT $ r a

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


-- search :: Term -> Telescope Term Term -> Maybe (Var, Term)
-- search t tl = Tl.find (aeq t) tl

-- | Given a judgement, create a goal/hole pair
goal :: (MonadRule m, MonadWriter (Telescope Extract Judgement) m) => Judgement -> m Term
goal j = do
    x <- metavar wildcard
    tell $ Tl.singleton x j
    return $ hole x
    -- return ((x, j), hole x)

-- | Infers the universe level of a given term
inferUniverse :: (MonadRule m) => Term -> m Int
inferUniverse (Universe k) = return $ k + 1
inferUniverse (Pi bnd) = lunbind bnd $ \(_, a, b) -> max <$> inferUniverse a <*> inferUniverse b
inferUniverse (Equals _ _ a) = inferUniverse a
inferUniverse _ = return 0

-- | Create a well-formedness goal
wellFormed :: (MonadRule m, MonadWriter (Telescope Extract Judgement) m) => Telescope Term Term -> Term -> m ()
wellFormed s t = do
    k <- inferUniverse t
    _ <- goal (s |- Equals t t (Universe k))
    return ()

ruleMismatch :: (MonadRule m, MonadError RuleError m) => Text -> Judgement -> m a
ruleMismatch t jdg = throwError $ RuleMismatch t jdg


-- | Creates a rule from a function. The WriterT is used to construct the subgoals listing
mkRule :: (MonadRule m) => (Telescope Term Term -> Term -> WriterT (Telescope Extract Judgement) (ExceptT RuleError m) Term) -> Rule m Judgement
mkRule f = Rule $ \j -> lunbind j $ \(hyps, g) -> do
    (extract, subgoals) <- runWriterT $ f hyps g
    return (subgoals |>> extract)

ruleError :: (MonadRule m) => RuleError -> Rule m Judgement
ruleError err = mkRule $ \_ _ -> throwError err

assumption :: (MonadRule m) => Rule m Judgement
assumption = mkRule $ \hyp goal -> 
        case (Tl.find (aeq goal) hyp) of
            Just (x,_) -> return $ Var x
            Nothing -> throwError $ NotInContext goal

-- | Prove a proposition by providing evidence
-- evidence :: (MonadRule m) => Term -> Rule m Judgement
-- evidence t = mkRule $ \hyp g -> do
--     let t' = Tl.withTelescope hyp t
--     (g', _) <- goal (hyp |- (Equals t' t' g))
--     return ((Tl.empty @> g') |> t')