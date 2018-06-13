{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Tactic where

import Control.Monad.Except
import Control.Monad.Reader

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import Data.Text (Text)

import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))

import MuPRL.Error
import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule

import qualified MuPRL.Refine.Rules.Equality as Equality
import qualified MuPRL.Refine.Rules.Function as Function
import qualified MuPRL.Refine.Rules.Universe as Universe
import qualified MuPRL.Refine.Rules.Void as Void

data TacticError 
    = RuleError RuleError
    | CustomError Text

instance Error TacticError where
    errorText (RuleError r) = errorText r
    errorText (CustomError t) = pretty t

type TacticT m = ExceptT TacticError m

newtype Tactic m a = Tactic { unTactic :: a -> TacticT m (ProofState a) }

runTactic :: (Monad m) => Judgement -> Tactic m Judgement -> m (Either TacticError (ProofState Judgement))
runTactic j (Tactic t) = runExceptT (t j)

applyTac :: (Fresh m) => Tactic m Judgement -> Judgement -> ExceptT TacticError m (Telescope Judgement, Term)
applyTac (Tactic t) j = do
    (ProofState s) <- t j
    unbind s

-- | Lifts a rule to the tactic level
ruleToTac :: (Monad m) => Rule m a -> Tactic m a
ruleToTac r = Tactic $ \j -> do
    ss <- lift $ runRule j r
    case ss of
        Left e -> throwError $ RuleError e
        Right r -> return r

-- | Creates a tactic that applies the rule with the given name
rule :: (MonadRule m) => Text -> Tactic m Judgement
rule name = ruleToTac $ mkRule $ case name of
    "eq/intro" -> Equality.intro
    "fun/intro" -> Function.intro
    "fun/eqtype" -> Function.eqType
    "universe/eqtype" -> Universe.eqType
    "void/eqtype" -> Void.eqType
    "assumption" -> assumption
    t -> ruleError $ NoSuchRule t

-- | Helper function for constructing tactics that examine the goal
goalTactic :: (Fresh m) => (Term -> (Telescope Term -> Term -> ExceptT RuleError m (ProofState Judgement))) -> Tactic m Judgement
goalTactic sel = Tactic $ \j@(Judgement bnd) -> do
    (_, goal) <- unbind bnd
    let (Tactic tac) = ruleToTac $ mkRule $ sel goal
    tac j

-- | Looks at the goal, and selects the proper introduction rule to use
intro :: (Fresh m) => Tactic m Judgement
intro = goalTactic $ \case
    (Equals (Var _) (Var _) _) -> Equality.intro
    (Pi _) -> Function.intro
    goal -> ruleError $ RuleMismatch "intro" goal

-- | Looks at the goal, and selects the proper eqType rule
eqType :: (Fresh m) => Tactic m Judgement
eqType = goalTactic $ \case
    (Pi _) -> Function.eqType
    (Universe _) -> Universe.eqType
    Void -> Void.eqType
    goal -> ruleError $ RuleMismatch "eqtype" goal

    
-- | Identity Tactic
idt :: (Fresh m) => Tactic m Judgement
idt = Tactic $ \j -> return' j
    
orElse :: (Monad m) => Tactic m a -> Tactic m a -> Tactic m a
orElse (Tactic t1) (Tactic t2) = Tactic $ \j -> catchError (t1 j) (const $ t2 j)

-- | Attempts to run a tactic, and backtracks if it fails
try :: (Monad m, Fresh m) => Tactic m Judgement -> Tactic m Judgement 
try t = t `orElse` idt

-- | Always fails with provided message
fail :: (Monad m) => Text -> Tactic m Judgement
fail t = Tactic $ \_ -> throwError $ CustomError t

-- | Applies a multi-tactic to the resu[Tactic m Judgement] -> lting goals of the 1st tactic
seq_ :: (Fresh m) => Tactic m Judgement -> Tactic m (ProofState Judgement) -> Tactic m Judgement
seq_ (Tactic t1) (Tactic t2) = Tactic $ \j -> do
    s <- t1 j
    s' <- t2 s
    join' s'


-- | Creates a multitactic that applies the given tactic to all of the subgoals
all_ :: forall m. (Fresh m) => Tactic m Judgement -> Tactic m (ProofState Judgement)
all_ t = Tactic $ \(ProofState bnd) -> do
    (goals, extract) <- unbind bnd
    s <- Tl.traverse (unTactic t) goals
    return (s |> extract)

-- | Applies the 1st tactic, then applies the 2nd tactic to all of the remaining goals
then_ :: (Fresh m) => Tactic m Judgement -> Tactic m Judgement -> Tactic m Judgement
then_ t1 t2 = seq_ t1 (all_ t2)

thenEach :: (Fresh m) => Tactic m Judgement -> [Tactic m Judgement] -> Tactic m Judgement
thenEach t1 ts = seq_ t1 (each ts)

-- | Given a list of tactics [t1, ..., tn], create a tactic that when given a proofstate [j1 ... jn], will run ti on ji
-- | If there are less tactics than goals, apply the identity tactic to the remaining ones
each :: forall m. (Fresh m) => [Tactic m Judgement] -> Tactic m (ProofState Judgement)
each ts = Tactic $ \(ProofState bnd) -> do
    (goals, extract) <- unbind bnd
    (_, s) <- Tl.foldrMWithKey applyTacs (reverse ts, Tl.empty) goals
    return (s |> extract)
    where
        applyTacs :: Name Term -> Judgement -> ([Tactic m Judgement], Telescope (ProofState Judgement)) -> ExceptT TacticError m ([Tactic m Judgement], Telescope (ProofState Judgement))
        applyTacs x xj (t:ts, tl) = do
            xs <- unTactic t xj
            return (ts, tl @> (x, xs))
        applyTacs x xj ([], tl) = do
            xs <- unTactic idt xj
            return ([], tl @> (x,xs))

many :: (Fresh m) => Tactic m Judgement -> Tactic m Judgement
many t = try (t `then_` (many t))