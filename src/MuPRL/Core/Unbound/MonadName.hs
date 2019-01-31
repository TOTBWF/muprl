{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MuPRL.Core.Unbound.MonadName 
    ( MonadName(..)
    , NameMT(..), NameM, runNameMT, runNameM
    , LocalBind(..)
    , GlobalBind(..)
    )
where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader

import Refinery.Tactic (MonadExtract(..))
import Refinery.Tactic.Internal (TacticT, mapTacticT, RuleT, mapRuleT)

import Unbound.Generics.LocallyNameless (LFreshMT, FreshMT, LFresh, Fresh, Bind, Embed, Alpha, Subst)
import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Fresh as Unbound

import MuPRL.Core.Term


class (Fresh m, LFresh m) => MonadName m where
    -- | Creates a locally fresh variable
    var :: String -> m Var
    default var :: (MonadTrans t, MonadName m1, t m1 ~ m) => String -> m Var
    var = lift . var
    -- | Creates a globally fresh metavariable
    metavar :: String -> m MetaVar
    default metavar :: (MonadTrans t, MonadName m1, t m1 ~ m) => String -> m MetaVar
    metavar = lift . metavar

newtype NameMT m a = NameMT { unNameMT :: LFreshMT (FreshMT m) a}
    deriving (Functor, Applicative, Monad, MonadIO, Fresh, LFresh, MonadError e)

type NameM = NameMT Identity

instance MonadTrans NameMT where
    lift = NameMT . lift . lift

instance (Fresh m) => Fresh (LFreshMT m) where
    fresh = lift . Unbound.fresh

instance (Fresh m) => Fresh (RuleT jdg ext m) where
  fresh = lift . Unbound.fresh

instance (LFresh m) => LFresh (RuleT jdg ext m) where
  lfresh = lift . Unbound.lfresh
  avoid = mapRuleT . Unbound.avoid
  getAvoids = lift Unbound.getAvoids

instance (Fresh m) => Fresh (TacticT jdg ext m) where
  fresh = lift. Unbound.fresh

instance (LFresh m) => LFresh (TacticT jdg ext m) where
  lfresh = lift . Unbound.lfresh
  avoid = mapTacticT . Unbound.avoid
  getAvoids = lift Unbound.getAvoids

instance (Monad m) => MonadName (NameMT m) where
    var = NameMT . Unbound.lfresh . Unbound.s2n
    metavar = NameMT . Unbound.fresh . Unbound.s2n

runNameMT :: (Monad m) => NameMT m a -> m a
runNameMT = Unbound.runFreshMT . Unbound.runLFreshMT . unNameMT

runNameM :: NameM a -> a
runNameM = runIdentity . runNameMT

class Bindable b where
    bind :: (Alpha p, Alpha t) => p -> t -> b p t

class LocalBind b u | b -> u where
    lunbind :: (MonadName m) => b -> (u -> m a) -> m a

instance LocalBind (Bind Var Term) (Var, Term) where
    lunbind = Unbound.lunbind

instance LocalBind (Bind (Var, Embed Term) Term) (Var, Term, Term) where
    lunbind bnd cont = Unbound.lunbind bnd (\((x, a), b) -> cont (x, Unbound.unembed a, b))

class GlobalBind b u | b -> u where
    unbind :: (MonadName m) => b -> m u


instance (Monad m) => MonadExtract Term (NameMT m) where
  hole = Hole <$> metavar wildcard

{- MTL Boilerplate -}
instance (MonadName m) => MonadName (ExceptT e m)
instance (MonadName m) => MonadName (ReaderT r m)
instance (MonadName m) => MonadName (RuleT jdg ext m)
instance (MonadName m) => MonadName (TacticT jdg ext m)
