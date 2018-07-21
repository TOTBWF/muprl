module MuPRL.Refine.Judgement where

import Data.Typeable (Typeable)
import GHC.Generics

import qualified Unbound.Generics.LocallyNameless as Unbound

import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName

import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl


-- | A Judgement is a sequence of hypotheses (In the form of variables bound to terms), 
-- | along with a goal that may take its variables from the hypotheses
newtype Judgement = Judgement (Bind (Telescope Term Term) Term)
    deriving (Show, Typeable, Generic)

instance Alpha Judgement

instance Subst Term Judgement
instance Subst Extract Judgement

instance LocalBind Judgement (Telescope Term Term, Term) where
    lunbind (Judgement bnd) = Unbound.lunbind bnd

instance PrettyM Judgement where
    prettyM j = lunbind j $ \(hyps, goal) -> do
        pctx <- prettyM hyps
        pgoal <- prettyM goal
        return $ pctx <+> pretty "⊢" <+> pgoal

instance Pretty Judgement where
    pretty = runNameM . prettyM

(|-) :: Telescope Term Term -> Term -> Judgement
hyps |- goal = Judgement (bind hyps goal)
