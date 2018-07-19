module MuPRL.Refine.Judgement where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Data.Typeable (Typeable)
import GHC.Generics

import MuPRL.PrettyPrint

import MuPRL.Core.Term

import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl


-- | A Judgement is a sequence of hypotheses (In the form of variables bound to terms), 
-- | along with a goal that may take its variables from the hypotheses
newtype Judgement = Judgement (Bind (Telescope Term Term) Term)
    deriving (Show, Typeable, Generic)

instance Alpha Judgement

instance Subst Term Judgement
instance Subst Extract Judgement

instance PrettyM Judgement where
    prettyM (Judgement bnd) = lunbind bnd $ \(hyps, goal) -> do
        pctx <- prettyM hyps
        pgoal <- prettyM goal
        return $ pctx <+> pretty "⊢" <+> pgoal

instance Pretty Judgement where
    pretty = runLFreshM . prettyM

(|-) :: Telescope Term Term -> Term -> Judgement
hyps |- goal = Judgement (bind hyps goal)

-- substJdg :: (Fresh m) => MetaVar -> Extract -> Judgement -> m Judgement
-- substJdg x b (Judgement bnd) = do
--     (hyp, goal) <- unbind bnd
--     let hyps' = Tl.map (subst x b) hyp
--     let goal' = subst x b goal
--     return (hyps' |- goal')