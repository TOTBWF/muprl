module MuPRL.Refine.Judgements where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Data.Typeable (Typeable)
import GHC.Generics

import MuPRL.PrettyPrint

import MuPRL.Core.Term

import MuPRL.Refine.Telescope (Telescope)
import qualified MuPRL.Refine.Telescope as Tl



-- | A Judgement is a sequence of hypotheses (In the form of variables bound to terms), 
-- | along with a goal that may take its variables from the hypotheses
newtype Judgement = Judgement (Bind (Telescope Term) Term)
    deriving (Show, Typeable, Generic)

instance Alpha Judgement

instance PrettyM Judgement where
    prettyM (Judgement bnd) = lunbind bnd $ \(hyps, goal) -> do
        pctx <- traverse (\(x,xt) -> fmap (\pxt -> pretty x <> pretty ":" <> pxt) $ prettyM xt) $ Tl.toList hyps
        pgoal <- prettyM goal
        return $ hsep (punctuate comma pctx) <+> pretty "⊢" <+> pgoal

instance Pretty Judgement where
    pretty = runLFreshM . prettyM

(|-) :: Telescope Term -> Term -> Judgement
hyps |- goal = Judgement (bind hyps goal)

substJdg :: (Fresh m) => Name Term -> Term -> Judgement -> m Judgement
substJdg x b (Judgement bnd) = do
    (hyp, goal) <- unbind bnd
    let hyps' = Tl.map (subst x b) hyp
    let goal' = subst x b goal
    return (hyps' |- goal')