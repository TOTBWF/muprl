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
import MuPRL.Refine.Judgements
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


-- | The Proof State consists of a sequence of variables bound to judgements,
-- | (where the variable references the extract of the judgement).
-- | Each judgement can take free variables from earlier in the sequence.
-- | It also contains an evidence term that takes its free variables from the sequence.
newtype ProofState a = ProofState { unProofState :: (Bind (Telescope a) Term) }
    deriving (Show, Generic)

instance (Typeable a, Alpha a) => Alpha (ProofState a)

instance (Subst Term t, Typeable t, Alpha t) => Subst Term (ProofState t)

(|>) :: (Typeable a, Alpha a) => Telescope a -> Term -> ProofState a
ctx |> extract = ProofState (bind ctx extract)

-- ProofStates form a monad, but only over proper judgement types.
return' :: (Fresh m) => Judgement -> m (ProofState Judgement)
return' j = do
    x <- metavar
    return (Tl.singleton x j |> Var x)

join' :: forall m. (Fresh m) => ProofState (ProofState Judgement) -> m (ProofState Judgement)
join' (ProofState bnd) = do
    (goals, extract) <- unbind bnd
    -- TODO: This feels like a right fold could make things easier, need to think about it
    (goals', extract') <- Tl.foldlMWithKey applySubst (Tl.empty, extract) goals
    return (goals' |> extract')
    where
        applySubst :: (Telescope Judgement, Term) -> Name Term -> ProofState Judgement -> m (Telescope Judgement, Term)   
        applySubst (tl, a) x (ProofState bnd) = do
            (tlx, ax) <- unbind bnd
            tl' <- Tl.traverse (substJdg x ax) tl
            let a' = subst x ax a
            return (tlx `Tl.concat` tl', a')

-- | Helper function for axiomatic evidence
axiomatic :: (Typeable a, Alpha a) => ProofState a
axiomatic = Tl.empty |> Axiom