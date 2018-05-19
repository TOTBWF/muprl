module MuPRL.Refine.Telescope where

import Prelude hiding (foldr, foldl)

import Control.Monad.Reader

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.PrettyPrint

import MuPRL.Core.Term

-- | 'v' is the type that the meta-vars refer to, and t is the type actually stored inside the telescope
data Telescope t
    = Empty
    | SnocHyp (Rebind (Telescope t) (Name Term, Embed t))
    deriving (Show, Generic, Typeable)

instance (Typeable t, Alpha t) => Alpha (Telescope t)

instance (Subst t t) => Subst t (Telescope t) where

instance (Show t) => Pretty (Telescope t) where
    pretty t = pretty $ show t

empty :: Telescope t
empty = Empty

singleton :: (Typeable t, Alpha t) => Name Term -> t -> Telescope t
singleton x xt = extend x xt empty


extend :: (Typeable t, Alpha t) => Name Term -> t -> Telescope t -> Telescope t
extend x xt tl = SnocHyp (rebind tl (x, embed xt))

-- | Infix, flipped, uncurried version of extend extend
(@>) :: (Typeable t, Alpha t) => Telescope t -> (Name Term, t) -> Telescope t
tl @> (x, xt) = extend x xt tl

find :: (Typeable t, Alpha t) => (t -> Bool) -> Telescope t -> Maybe (Name Term, t)
find _ Empty = Nothing
find p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p xt = Just (x, xt)
                                                        | otherwise = find p tl

anyKey :: (Typeable t, Alpha t) => (Name Term -> t -> Bool) -> Telescope t -> Bool
anyKey _ Empty = False
anyKey p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p x xt = True
                                                          | otherwise = anyKey p tl

-- Note that Telescope v is not an instance of Foldable, as we can only fold when the elements can be compared using α-equivalence
foldr :: (Typeable a, Alpha a) => (a -> b -> b) -> b -> Telescope a -> b
foldr _ b Empty = b
foldr f b (SnocHyp (unrebind -> (tl, (_, unembed -> xt)))) = f xt (foldr f b tl)

foldl :: (Typeable a, Alpha a) => (b -> a -> b) -> b -> Telescope a -> b
foldl _ b Empty = b
foldl f b (SnocHyp (unrebind -> (tl, (_, unembed -> xt)))) = foldl f (f b xt) tl

foldrWithKey :: (Typeable a, Alpha a) => (Name Term -> a -> b -> b) -> b -> Telescope a -> b
foldrWithKey _ b Empty = b
foldrWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = f x xt (foldrWithKey f b tl)

foldlWithKey :: (Typeable a, Alpha a) => (b -> Name Term -> a -> b) -> b -> Telescope a -> b
foldlWithKey _ b Empty = b
foldlWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = foldlWithKey f (f b x xt) tl

foldMWithKey :: (Typeable a, Alpha a, Monad m) => (b -> Name Term -> a -> m b) -> b -> Telescope a -> m b
foldMWithKey _ b Empty = return b
foldMWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = do
    b' <- f b x xt
    foldMWithKey f b' tl

toList :: (Typeable a, Alpha a) => Telescope a -> [(Name Term, a)]
toList = foldrWithKey (\x xt xs-> (x,xt):xs) []

-- | TODO: This may be broken
withTelescope :: (Subst Term t) => Telescope Term -> t -> t
withTelescope Empty t = t
withTelescope (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) t = 
    let t' = subst x xt t
        tl' = subst x xt tl
    in withTelescope tl' t'
-- withTelescope v (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) t = 
--     let t' = subst x xt t
    -- in withTelescope v tl t

-- withTelescope v (Empty) cont = cont $ Map.empty
-- newtype Telescope v a = Telescope v { unTelescope v :: Seq (MetaVar, a) }
--     deriving (Functor, Traversable, Foldable)


-- foldrVars :: (MetaVar -> a -> b -> b) -> b -> Telescope v a -> b
-- foldrVars f b (Telescope v t) = foldr (uncurry f) b t

-- foldlVars :: (b -> MetaVar -> a -> b) -> b -> Telescope v a -> b
-- foldlVars f b (Telescope v t) = foldl (\b (x,a) -> f b x a) b t

-- foldMVars :: (Monad m) => (b -> MetaVar -> a -> m b) -> b -> Telescope v a -> m b
-- foldMVars f b (Telescope v t) = foldM (\b (x,a) -> f b x a) b t

-- traverseVars :: (Applicative f) => (MetaVar -> a -> f b) -> Telescope v a -> f (Telescope v b)
-- traverseVars f (Telescope v t) = Telescope v <$> traverse (\(x,a) -> (x,) <$> f x a) t

-- extend:: Telescope v a -> MetaVar -> a -> Telescope v a
-- extend (Telescope v s) x t = Telescope v $ s :|> (x,t)

-- append :: Telescope v a -> Telescope v a -> Telescope v a
-- append (Telescope v a) (Telescope v b) = Telescope v (a >< b)

-- empty :: Telescope v a
-- empty = Telescope v $ Seq.empty

-- singleton :: MetaVar -> a -> Telescope v a
-- singleton x a = Telescope v $ Seq.singleton (x, a)

-- fromList :: [(MetaVar,a)] -> Telescope v a
-- fromList = Telescope v . Seq.fromList

-- toList :: Telescope v a -> [(MetaVar, a)]
-- toList (Telescope v t) = F.toList t

-- find :: (a -> Bool) -> Telescope v a -> Maybe (MetaVar, a)
-- find p (Telescope v tl) = F.find (p . snd) tl
-- -- lookup :: Int -> Telescope v a -> Maybe a
-- -- lookup i (Telescope v tl) = snd <$> Seq.lookup i tl