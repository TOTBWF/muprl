{-# LANGUAGE ConstraintKinds #-}
module MuPRL.Refine.Rules where

import Control.Monad.Except
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Error
import MuPRL.PrettyPrint
import MuPRL.Core.Term
import MuPRL.Refine.ProofState
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
    -- let ms = fst <$> Tl.toList ctx
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

-- member :: (Fresh m) => Term -> m Term
-- member t = do
--     k <- inferUniverse t
--     return (EqType t t (Universe k))

-- | Create a well-formedness goal
wellFormed :: (Fresh m) => Telescope Term -> Term -> m ((MetaVar, Judgement), Extract)
wellFormed s t = do
    k <- inferUniverse t
    goal (s |- Equals t t (Universe k))


ruleMismatch :: (MonadError RuleError m) => Judgement -> m a
ruleMismatch jdg = throwError $ RuleMismatch jdg

mkRule :: (MonadRule m) => (Telescope Term -> Term -> ExceptT RuleError m (ProofState Judgement)) -> Rule m Judgement
mkRule f = Rule $ \(Judgement bnd) -> do
    (hyps, goal) <- unbind bnd
    f hyps goal

assumption :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
assumption hyp goal =
        case search goal hyp of
            Just (x,_) -> return (Tl.empty |> Var x)
            Nothing -> throwError $ NotInContext goal

intro :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
intro _ (Equals Void Void (Universe _)) = return axiomatic
intro _ (Equals (Universe i) (Universe j) (Universe k)) | (max i j < k) = return axiomatic
                                                        | otherwise = throwError $ UniverseMismatch (max i j) k
intro hyp (Pi bnd) = do
    ((x, unembed -> a), bx) <- unbind bnd
    -- We first need to check the well formedness of 'a'
    (wGoal, _) <- wellFormed hyp a
    -- p <- member a
    (bGoal, body) <- goal (Tl.extend x a hyp |- bx)
    return ((Tl.empty @> wGoal @> bGoal) |> lambda x body)
intro hyps goal = ruleMismatch (hyps |- goal)
    -- We first need to check the well formedness of 'a'

    -- (_ :>> Equals Void Void (Universe _)) -> axiomatic
    -- (_ :>> Equals Axiom Axiom (Universe _)) -> axiomatic
    -- -- Functions
    -- (ctx :>> Pi bnd) -> do
    --     ((x, unembed -> a), bx) <- unbind bnd
    --     -- We first need to check the well formedness of 'a'
    --     (wGoal, j) <- wellFormed ctx a
    --     (bGoal, body) <- goal (Tl.extend ctx x a :>> bx)
    --     return ((Tl.empty @> bGoal @> wGoal) :#> lambda x body)
    -- jdg -> ruleMismatch jdg