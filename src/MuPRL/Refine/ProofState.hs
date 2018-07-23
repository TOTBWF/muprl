module MuPRL.Refine.ProofState where

import Control.Monad.Trans
import Control.Monad.Except
import qualified Data.Set as Set
import Data.Set (Set(..))

import Data.Typeable (Typeable)
import GHC.Generics

import qualified Unbound.Generics.LocallyNameless as Unbound

import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Core.Telescope (Telescope)
import MuPRL.Refine.Judgement
import qualified MuPRL.Core.Telescope as Tl

import Debug.Trace

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
newtype ProofState a = ProofState { unProofState :: (Bind (Telescope Extract a) Extract) }
    deriving (Show, Generic)

instance (Typeable a, Alpha a) => GlobalBind (ProofState a) (Telescope Extract a, Extract) where
    unbind (ProofState bnd) = Unbound.unbind bnd

instance (Typeable a, Alpha a) => Alpha (ProofState a)

instance (Subst Term t, Typeable t, Alpha t) => Subst Term (ProofState t)

instance (PrettyM a, Typeable a, Alpha a) => PrettyM (ProofState a) where
    prettyM p = do
        (subgoals, Extract extract) <- unbind p
        psubgoals <- prettyM subgoals
        pextract <- prettyM extract
        return $ psubgoals <+> pretty "▹" <+> pextract

instance (PrettyM a, Typeable a, Alpha a) => Pretty (ProofState a) where
    pretty = runNameM . prettyM

(|>>) :: (Typeable a, Alpha a) => Telescope Extract a -> Term -> ProofState a
ctx |>> extract = ProofState (bind ctx (Extract extract))

(|>) :: (Typeable a, Alpha a) => Telescope Extract a -> Extract -> ProofState a
ctx |> extract = ProofState (bind ctx extract)

-- ProofStates form a monad, but only over proper judgement types.
return' :: (MonadName m) => Judgement -> m (ProofState Judgement)
return' j = do
    x <- metavar wildcard
    return $ Tl.singleton x j |> (Extract (hole x))

join' :: forall m. (MonadName m) => ProofState (ProofState Judgement) -> m (ProofState Judgement)
join' p = do
    (goals, extract) <- unbind p
    (goals', env, extract') <- Tl.foldrMWithKey buildSubst (Tl.empty, [], extract) goals
    trace "-------------------------"
        $ trace ("Goals: " ++ (show $ pretty goals))
        $ trace ("Extract: " ++ (show $ unExtract extract)) 
        $ trace ("Env :" ++ show env)
        $ trace ("Goals (Subst): " ++ (show $ pretty goals'))
        $ trace ("Extract (Subst): " ++ (show $ unExtract $ extract')) 
        $ return (goals' |> substs env extract')
    -- return (goals' |> substs env extract)
    where
        buildSubst :: MetaVar -> ProofState Judgement -> (Telescope Extract Judgement, [(MetaVar, Extract)], Extract) -> m (Telescope Extract Judgement, [(MetaVar, Extract)], Extract)
        buildSubst x p (tl, env, extract) = do
            (tlx, ax) <- unbind p
            let tlx' = substs env tlx
            let extract' = subst x ax extract
            return (tl `Tl.concat` tlx', (x,ax):env, extract')

-- | Helper function for axiomatic evidence
axiomatic :: (Typeable a, Alpha a) => ProofState a
axiomatic = Tl.empty |> (Extract Axiom)