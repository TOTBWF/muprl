module MuPRL.Refine.ProofState where

import Control.Monad.Trans
import Control.Monad.Except
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Refine.Telescope

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

-- | A Judgement is a sequence of Hypotheses (In the form of variables bound to terms), along with a goal
-- instance Pretty Judgement where
--     ppr (ctx :>> t) = do
--         pctx <- foldM (\p (x,t') -> do
--                 px <- ppr x
--                 pt' <- ppr t'
--                 return $ px <> text ":" <> pt' <> text "," <+> p) (text "") ctx
--         pt <- ppr t
--         return $ pctx <> text "H >>" <+> pt
data Judgement = (Telescope Term) :>> Term

instance PrettyFresh Judgement where
    prettyFresh (ctx :>> j) = do
        pctx <- traverse (\(x, t) -> fmap (\pt -> pretty x <> pretty ":" <> pt) $ prettyFresh t) $ toList ctx
        pt <- prettyFresh j
        return $ hsep (punctuate comma pctx) <> pretty " >>" <+> pt

instance Pretty Judgement where
    pretty = runLFreshM . prettyFresh



substJdg :: (Subst b Term) => Name b -> b -> Judgement -> Judgement
substJdg x b (ctx :>> g) = (fmap (subst x b) ctx :>> subst x b g)

-- | The Proof State consists of a sequence of variables bound to judgements,
-- | (where the variable references the extract of the judgement).
-- | Each judgement can take free variables from earlier in the sequence.
-- | It also contains an evidence term that takes its free variables from the sequence.
data ProofState a = Telescope a :#> Extract


-- ProofStates form a monad, but only over proper judgement types.
wrap :: (Fresh m) => Judgement -> m (ProofState Judgement)
wrap j = do
    x <- metavar
    return (singleton x j :#> Hole x)

collapse :: ProofState (ProofState Judgement) -> ProofState Judgement
collapse (tl :#> extract) = foldrVars go (empty :#> extract) tl
    where
        go :: MetaVar -> ProofState Judgement -> ProofState Judgement -> ProofState Judgement
        go x (tlx :#> ax) (tl :#> a) = 
            let tl' = fmap (substJdg x ax) tl
                a' = subst x ax a
            in (tl' :#> a')

-- | Helper function for axiomatic evidence
axiomatic :: (Monad m) => m (ProofState a)
axiomatic = return (empty :#> Axiom)