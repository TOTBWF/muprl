module MuPRL.Refine.Telescope where

import Control.Monad(foldM)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence.Internal
import Data.Sequence (Seq(..), (><))

import Unbound.Generics.LocallyNameless

import MuPRL.Core.Term

newtype Telescope a = Telescope { unTelescope :: Seq (MetaVar, a) }
    deriving (Functor, Traversable, Foldable)


foldrVars :: (MetaVar -> a -> b -> b) -> b -> Telescope a -> b
foldrVars f b (Telescope t) = foldr (uncurry f) b t

foldlVars :: (b -> MetaVar -> a -> b) -> b -> Telescope a -> b
foldlVars f b (Telescope t) = foldl (\b (x,a) -> f b x a) b t

foldMVars :: (Monad m) => (b -> MetaVar -> a -> m b) -> b -> Telescope a -> m b
foldMVars f b (Telescope t) = foldM (\b (x,a) -> f b x a) b t

traverseVars :: (Applicative f) => (MetaVar -> a -> f b) -> Telescope a -> f (Telescope b)
traverseVars f (Telescope t) = Telescope <$> traverse (\(x,a) -> (x,) <$> f x a) t

extend :: Telescope a -> MetaVar -> a -> Telescope a
extend (Telescope s) x t = Telescope $ s :|> (x,t)

append :: Telescope a -> Telescope a -> Telescope a
append (Telescope a) (Telescope b) = Telescope (a >< b)

empty :: Telescope a
empty = Telescope $ Seq.empty

singleton :: MetaVar -> a -> Telescope a
singleton x a = Telescope $ Seq.singleton (x, a)

fromList :: [(MetaVar,a)] -> Telescope a
fromList = Telescope . Seq.fromList

toList :: Telescope a -> [(MetaVar, a)]
toList (Telescope t) = F.toList t

find :: (a -> Bool) -> Telescope a -> Maybe (MetaVar, a)
find p (Telescope tl) = F.find (p . snd) tl
-- lookup :: Int -> Telescope a -> Maybe a
-- lookup i (Telescope tl) = snd <$> Seq.lookup i tl