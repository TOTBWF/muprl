module MuPRL.Core.Eval where

import Control.Monad.Except

import Data.Set (Set)
import qualified Data.Set as Set

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import Data.Text.Prettyprint.Doc

import MuPRL.Core.Term
import MuPRL.Error

data EvalError 
    = NotRedex Term
    | TermNotClosed Term

instance Error EvalError where
    errorText (NotRedex t) = pretty t <+> pretty "is not a redex"
    errorText (TermNotClosed t) = pretty t <+> pretty "is not a closed term"

-- | Determines if a term is canonical form, that is, it cannot be reduced further
isCanonical :: Term -> Bool
isCanonical = \case
    (Var _) -> True
    Void -> True
    (Universe _) -> True
    (Lam _) -> True
    (Pi _) -> True
    (Equals _ _ _) -> True
    _ -> False

-- | Evaluates a redex by 1 step
evalRedex :: (Fresh m, MonadError EvalError m) => Term -> m Term
evalRedex (App (Lam bnd) a) = do
    (x, body) <- unbind bnd
    return $ subst x a body
evalRedex t = throwError $ NotRedex t

-- | Attempts to evaluate a term
eval :: (Fresh m, MonadError EvalError m) => Term -> m Term
eval t | fvSet t /= Set.empty = throwError $ TermNotClosed t
       | isCanonical t = return t
       | otherwise = case t of
            (App f a) -> do
                -- Make sure to evaluate the lambda term 1st
                f' <- eval f
                evalRedex (App f' a)