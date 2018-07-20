{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Tactic where

import Control.Monad.Except
import Control.Monad.Reader

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import Data.Text (Text)
import qualified Data.Text as T

import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))

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

import Debug.Trace

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

applyTac :: (Fresh m) => Tactic m Judgement -> Judgement -> ExceptT TacticError m (Telescope Extract Judgement, Extract)
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

-- TODO: Add support for directly invoking elimination rules
-- | Creates a tactic that applies the rule with the given name
rule :: (MonadRule m) => Text -> Tactic m Judgement
rule name = ruleToTac $ case name of
    "eq/intro" -> Equality.intro
    "fun/intro" -> Function.intro
    "fun/eq" -> Function.eq
    "fun/eqtype" -> Function.eqType
    "universe/eqtype" -> Universe.eqType
    "void/eqtype" -> Void.eqType
    "assumption" -> assumption
    t -> ruleError $ NoSuchRule t

-- | Helper function for constructing tactics that examine the goal
goalTactic :: (MonadRule m) => (Term -> Rule m Judgement) -> Tactic m Judgement
goalTactic sel = Tactic $ \j@(Judgement bnd) -> lunbind bnd $ \(_, goal) -> do
    let (Tactic tac) = ruleToTac $ sel goal
    tac j

-- | Helper function for constructing tactics that examine the hypotheses
hypTactic :: (MonadRule m) => (Telescope Term Term -> Rule m Judgement) -> Tactic m Judgement
hypTactic sel = Tactic $ \j@(Judgement bnd) -> lunbind bnd $ \(hyp, _) -> do
    let (Tactic tac) = ruleToTac $ sel hyp
    tac j

-- | Looks at the goal, and selects the proper introduction rule to use
intro :: (MonadRule m) => Tactic m Judgement
intro = goalTactic $ \case
    (Equals (Var _) (Var _) _) -> Equality.intro
    (Pi _) -> Function.intro
    goal -> ruleError $ GoalMismatch "intro" goal

-- | Looks at the goal, and selects the proper eqType rule
eqType :: (LFresh m) => Tactic m Judgement
eqType = goalTactic $ \case
    (Pi _) -> Function.eqType
    (Universe _) -> Universe.eqType
    Void -> Void.eqType
    goal -> ruleError $ GoalMismatch "eqtype" goal

-- | Looks at the goal, and selects the proper elimination rule
elim :: (LFresh m) => Text -> Tactic m Judgement
elim x = hypTactic $ \hyp -> 
    let x' = string2Name $ T.unpack x
    in case Tl.lookupKey x' hyp of
        (Just (Pi _)) -> Function.elim x'
        (Just goal) -> ruleError $ ElimMismatch "elim" goal
        Nothing -> ruleError $ UndefinedVariable (s2n $ T.unpack x)
    -- (hyp, goal) <- unbind bnd
    -- case Tl.findKey x hyp of
    --     Nothing -> ruleError $ UndefinedVariable x
    
-- | Identity Tactic
idt :: (LFresh m) => Tactic m Judgement
idt = Tactic $ \j -> return' j
    
orElse :: (Monad m) => Tactic m a -> Tactic m a -> Tactic m a
orElse (Tactic t1) (Tactic t2) = Tactic $ \j -> catchError (t1 j) (const $ t2 j)

-- | Attempts to run a tactic, and backtracks if it fails
try :: (Monad m, LFresh m) => Tactic m Judgement -> Tactic m Judgement 
try t = t `orElse` idt

-- | Always fails with provided message
fail :: (Monad m) => Text -> Tactic m Judgement
fail t = Tactic $ \_ -> throwError $ CustomError t

-- | Applies a multi-tactic to the resu[Tactic m Judgement] -> lting goals of the 1st tactic
seq_ :: (MonadRule m) => Tactic m Judgement -> Tactic m (ProofState Judgement) -> Tactic m Judgement
seq_ (Tactic t1) (Tactic t2) = Tactic $ \j -> do
    s <- t1 j
    s' <- t2 s
    join' s'


-- | Creates a multitactic that applies the given tactic to all of the subgoals
all_ :: forall m. (MonadRule m) => Tactic m Judgement -> Tactic m (ProofState Judgement)
all_ t = Tactic $ \(ProofState bnd) -> lunbind bnd $ \(goals, extract) -> do
    s <- Tl.traverse (unTactic t) goals
    return (s |> extract)

-- | Applies the 1st tactic, then applies the 2nd tactic to all of the remaining goals
then_ :: (MonadRule m) => Tactic m Judgement -> Tactic m Judgement -> Tactic m Judgement
then_ t1 t2 = seq_ t1 (all_ t2)

thenEach :: (MonadRule m) => Tactic m Judgement -> [Tactic m Judgement] -> Tactic m Judgement
thenEach t1 ts = seq_ t1 (each ts)

-- | Given a list of tactics [t1, ..., tn], create a tactic that when given a proofstate [j1 ... jn], will run ti on ji
-- | If there are less tactics than goals, apply the identity tactic to the remaining ones
each :: forall m. (MonadRule m) => [Tactic m Judgement] -> Tactic m (ProofState Judgement)
each ts = Tactic $ \(ProofState bnd) -> lunbind bnd $ \(goals, extract) -> do
    (_, s) <- Tl.foldrMWithKey applyTacs (ts, Tl.empty) goals
    return (s |> extract)
    where
        applyTacs :: MetaVar -> Judgement -> ([Tactic m Judgement], Telescope Extract (ProofState Judgement)) -> ExceptT TacticError m ([Tactic m Judgement], Telescope Extract (ProofState Judgement))
        applyTacs x xj (t:ts, tl) = do
            xs <- unTactic t xj
            return (ts, tl @> (x, xs))
        applyTacs x xj ([], tl) = do
            xs <- unTactic idt xj
            return ([], tl @> (x,xs))

many :: (MonadRule m) => Tactic m Judgement -> Tactic m Judgement
many t = try (t `then_` (many t))

-- -- | Use a term that takes its free variables from the hypotheses as evidence
-- use :: (Fresh m) => Term -> Tactic m Judgement
-- use = ruleToTac . evidence
-- use t = Tactic $ \(Judgement bnd) -> do
--     (hyps, goal) <- unbind bnd
--     let t' = Tl.withTelescope hyps t
--     undefined