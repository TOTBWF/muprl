{-# LANGUAGE ConstraintKinds #-}
module MuPRL.Refine.Rules where

import Control.Monad.Except
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Error
import MuPRL.PrettyPrint
import MuPRL.Core.Term
import MuPRL.Refine.ProofState
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope)

type MonadRule m = (Fresh m)

newtype Rule m a = Rule { unRule :: a -> ExceptT RuleError m (ProofState a) }

runRule :: a -> Rule m a -> m (Either RuleError (ProofState a))
runRule a (Rule r) = runExceptT $ r a

data RuleError 
    = UniverseMismatch Int Int
    | TypeMismatch Term Term
    | NotInContext Term
    | RuleMismatch Judgement

instance Error RuleError where
    errorText (UniverseMismatch i j) = pretty "Universe Mismatch:" <+> pretty i <+> pretty "and" <+> pretty j
    errorText (TypeMismatch t1 t2) = pretty "Type Mismatch:" <+> pretty t1 <+> pretty "and" <+> pretty t2
    errorText (NotInContext t) = pretty "Not In Context:" <+> pretty t
    errorText (RuleMismatch j) = pretty "Rule Mismatch:" <+> pretty j


search :: Term -> Telescope Term -> Maybe (MetaVar, Term)
search t tl = Tl.find (aeq t) tl

-- | Given a judgement, create a goal/hole pair
goal :: (Fresh m) => Judgement -> m ((MetaVar, Judgement), Extract)
goal a = do
    x <- metavar
    return ((x, a), Hole x)

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
    goal (s :>> Equals t t (Universe k))


ruleMismatch :: (MonadError RuleError m) => Judgement -> m a
ruleMismatch jdg = throwError $ RuleMismatch jdg

assumption :: (MonadRule m) => Rule m Judgement
assumption = Rule $ \case
    (ctx :>> a) -> 
        case search a ctx of
            Just (x, a') -> return (Tl.empty :#> (Hole x))
            Nothing -> throwError $ NotInContext a


intro :: (MonadRule m) => Rule m Judgement
intro = Rule $ \case
    -- Equality
    (_ :>> Equals Void Void (Universe _)) -> axiomatic
    (_ :>> Equals Axiom Axiom (Universe _)) -> axiomatic
    -- Functions
    (ctx :>> Pi bnd) -> do
        ((x, unembed -> a), b) <- unbind bnd
        -- We first need to check the well formedness of 'a'
        (wGoal, _) <- wellFormed ctx a
        (bGoal, body) <- goal (Tl.extend ctx x a :>> b)
        return (Tl.fromList [bGoal, wGoal] :#> lambda x body)
    jdg -> ruleMismatch jdg