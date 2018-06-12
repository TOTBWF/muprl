module MuPRL.Refine.Tactics where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import Data.Text (Text)

import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))

import MuPRL.Error
import MuPRL.PrettyPrint

import MuPRL.Core.Term
import MuPRL.Refine.ProofState
import MuPRL.Refine.Rules
import MuPRL.Refine.Judgements

data TacticError 
    = RuleError RuleError
    | CustomError Text

instance Error TacticError where
    errorText (RuleError r) = errorText r
    errorText (CustomError t) = pretty t

newtype Tactic m a = Tactic { unTactic :: a -> ExceptT TacticError m (ProofState a) }

runTac :: (Fresh m) => Tactic m Judgement -> Judgement -> ExceptT TacticError m (Telescope Judgement, Term)
runTac (Tactic t) j = do
    (ProofState s) <- t j
    unbind s

-- | Lifts a rule to the tactic level
rule :: (Monad m) => Rule m a -> Tactic m a
rule r = Tactic $ \j -> do
    ss <- lift $ runRule j r
    case ss of
        Left e -> throwError $ RuleError e
        Right r -> return r

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
--     (_, s, metavars) <- Tl.foldMWithKey applyTacs (ts, Tl.empty, Tl.empty) goals
--     let extract' = Tl.withTelescope metavars extract
--     return (s |> extract')
--     where
--         applyTacs :: ([Tactic m Judgement], Telescope (ProofState Judgement), Telescope Term) -> Name Term -> Judgement -> ExceptT TacticError m ([Tactic m Judgement], Telescope (ProofState Judgement), Telescope Term)
--         applyTacs (t:ts, tl, metavars) x xj = do
--             (jdg, mv) <- runTac t xj
--             return (ts, tl @> (x, (jdg |> mv)), metavars @> (x, mv))
--         applyTacs ([], tl, metavars) x xj = do
--             (jdg, mv) <- runTac idt xj
--             return ([], tl @> (x, (jdg |> mv)), metavars @> (x, mv))
