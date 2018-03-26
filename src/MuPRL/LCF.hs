{-# LANGUAGE KindSignatures #-}
module MuPRL.LCF where

import Unbound.Generics.LocallyNameless
import GHC.Generics
import MuPRL.Syntax
import Data.Bifunctor
import Control.Monad.Except
import qualified Data.Sequence as Seq
import Data.Sequence.Internal
import Data.Sequence (Seq(..))

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



data Judgement = Seq (Var, Term) :>> Term
    deriving (Show)

substJudgement :: Name Term -> Term -> Judgement -> Judgement
substJudgement v t (h :>> g) = fmap (subst v t) h :>> subst v t g

-- A proof state consists of a telescope of variables bound to judgements,
-- along with a term that takes its free variables from the telescope
data ProofState = Seq (Var, Judgement) :#> Term

-- Rules need to be able to handle fresh variable 
newtype Rule r = Rule { unRule :: Judgement -> r ProofState }

-- A Tactic lifts a rule into its own embellished monadic context
newtype Tactic (t :: (* -> *) -> * -> *) r = Tactic { unTactic :: Judgement -> t r ProofState }

-- Rule Creation

-- ^ The (Var, Judgement) term is the "goal"
-- ^ The Term is the "hole"
goal :: (Fresh m) => Judgement -> m ((Var, Judgement), Term)
goal jdg@(h :>> t) = do
    x <- fresh metavar
    return ((x, jdg), Var x)


axiomatic :: (Monad m) => m ProofState
axiomatic = return (Seq.empty :#> Axiom)

-- Tactic Combinators
rule :: (MonadTrans t, Monad r) => Rule r -> Tactic t r
rule (Rule r) = Tactic $ lift . r
