module MuPRL.Refine.ProofState where

import Control.Monad.Trans
import Control.Monad.Except
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Data.Typeable (Typeable)
import GHC.Generics

import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Refine.Telescope (Telescope)
import qualified MuPRL.Refine.Telescope as Tl

{-
A proof state is a telescope of judgements that bind metavars in the rest of the telescope
Along with a term that draws its free metavars from the telescope

The telescope takes the place of our subgoals, and the term takes the place of the validation

Keep in mind that where a variable stands in for some term,
A metavariable stands in for an abstraction/binder of any valence

For example, a variable would stand in for a value,
where a metavariable would stand in for some property about a value

(In our case, this variable would be a bit of evidence that will be filled in)

For example consider:

M in A      N in B[M]
----------------------
(M,N) in (x : A) * B[x]

The premises can be stated in terms of the conclusion, because M appears in the conclusion

Now consider this:

A         B[M]
--------------
(x : A) * B[x]

This should extract (M,N), but it can't! We can't state 'B[M]' until we have run the validation for the 1st proof.

Normally we would need to rephrase this as:

M in A     B[M]
--------------
(x : A) * B[x]

But this kind of sucks, as you need to provide a witness for A, and you can't proceed using refinement.

The way around this is to use Dependent LCF, which allows us to partially construct M as we further refine A.
-}

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

substJdg :: (Fresh m, Subst b Term) => Name b -> b -> Judgement -> m Judgement
substJdg x b (Judgement bnd) = do
    (hyp, goal) <- unbind bnd
    let hyps' = Tl.map (subst x b) hyp
    let goal' = subst x b goal
    return (hyps' |- goal')

-- | The Proof State consists of a sequence of variables bound to judgements,
-- | (where the variable references the extract of the judgement).
-- | Each judgement can take free variables from earlier in the sequence.
-- | It also contains an evidence term that takes its free variables from the sequence.
newtype ProofState a = ProofState (Bind (Telescope a) Term)
    deriving (Show, Generic)

instance (Typeable a, Alpha a) => Alpha (ProofState a)

(|>) :: (Typeable a, Alpha a) => Telescope a -> Term -> ProofState a
ctx |> extract = ProofState (bind ctx extract)

-- ProofStates form a monad, but only over proper judgement types.
wrap :: (Fresh m) => Judgement -> m (ProofState Judgement)
wrap j = do
    x <- metavar
    return (Tl.singleton x j |> Var x)

collapse :: forall m. (Fresh m) => ProofState (ProofState Judgement) -> m (ProofState Judgement)
collapse (ProofState bnd) = do
    (goals, extract) <- unbind bnd
    (goals', extract') <- Tl.foldMWithKey applySubst (Tl.empty, extract) goals
    return (goals' |> extract')
    where
        applySubst :: (Telescope Judgement, Term) -> Name Term -> ProofState Judgement -> m (Telescope Judgement, Term)   
        applySubst (tl, a) x (ProofState bnd) = do
            (tlx, ax) <- unbind bnd
            tl' <- Tl.traverse (substJdg x ax) tl
            let a' = subst x ax a
            return (tl `Tl.concat` tl', a')

-- | Helper function for axiomatic evidence
axiomatic :: (Typeable a, Alpha a) => ProofState a
axiomatic = Tl.empty |> Axiom