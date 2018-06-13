{-# LANGUAGE ConstraintKinds #-}
module MuPRL.Refine.Rule where

import Control.Monad.Except

import Data.Text (Text)

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Error
import MuPRL.PrettyPrint
import MuPRL.Core.Term
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))

type MonadRule m = (Fresh m)

newtype Rule m a = Rule { unRule :: a -> ExceptT RuleError m (ProofState a) }

runRule :: a -> Rule m a -> m (Either RuleError (ProofState a))
runRule a (Rule r) = runExceptT $ r a

data RuleError 
    = UniverseMismatch Int Int
    | TypeMismatch Term Term
    | NotInContext Term
    | RuleMismatch Text Term
    | NoSuchRule Text

instance Error RuleError where
    errorText (UniverseMismatch i j) = pretty "Universe Mismatch:" <+> pretty i <+> pretty "and" <+> pretty j
    errorText (TypeMismatch t1 t2) = pretty "Type Mismatch:" <+> pretty t1 <+> pretty "and" <+> pretty t2
    errorText (NotInContext t) = pretty "Not In Context:" <+> pretty t
    errorText (RuleMismatch t j) = pretty "Rule Mismatch:" <+> pretty t <+> pretty j
    errorText (NoSuchRule t) = pretty "No Such Rule:" <+> pretty t


search :: Term -> Telescope Term -> Maybe (MetaVar, Term)
search t tl = Tl.find (aeq t) tl

-- | Given a judgement, create a goal/hole pair
goal :: (Fresh m) => Judgement -> m ((MetaVar, Judgement), Extract)
goal a = do
    x <- metavar
    -- TODO: In theory, 'x' can reference variables in the judgement context
    return ((x, a), Var x)

-- | Infers the universe level of a given term
inferUniverse :: (Fresh m) => Term -> m Int
inferUniverse (Universe k) = return $ k + 1
inferUniverse (Pi bnd) = do
    ((_, unembed -> a), b) <- unbind bnd
    max <$> inferUniverse a <*> inferUniverse b
inferUniverse (Equals _ _ a) = inferUniverse a
inferUniverse _ = return 0

-- | Create a well-formedness goal
wellFormed :: (Fresh m) => Telescope Term -> Term -> m ((MetaVar, Judgement), Extract)
wellFormed s t = do
    k <- inferUniverse t
    goal (s |- Equals t t (Universe k))


ruleMismatch :: (MonadRule m, MonadError RuleError m) => Text -> Judgement -> m a
ruleMismatch t (Judgement bnd) = do
    (_, goal) <- unbind bnd
    throwError $ RuleMismatch t goal

mkRule :: (MonadRule m) => (Telescope Term -> Term -> ExceptT RuleError m (ProofState Judgement)) -> Rule m Judgement
mkRule f = Rule $ \(Judgement bnd) -> do
    (hyps, goal) <- unbind bnd
    f hyps goal

ruleError :: (MonadRule m) => RuleError -> (Telescope Term -> Term -> ExceptT RuleError m (ProofState Judgement))
ruleError err = \_ _ -> throwError err

assumption :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
assumption hyp goal =
        case search goal hyp of
            Just (x,_) -> return (Tl.empty |> Var x)
            Nothing -> throwError $ NotInContext goal