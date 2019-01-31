module MuPRL.Core.Telescope where

import Prelude hiding (foldr, foldl, map, concat, traverse)

import Unbound.Generics.LocallyNameless

import GHC.Generics
import Data.Typeable (Typeable)

-- | 'v' is the type that the meta-vars refer to, and t is the type actually stored inside the telescope
data Telescope v t
    = Empty
    | SnocHyp (Rebind (Telescope v t) (Name v, Embed t))
    deriving (Show, Generic, Typeable)

instance (Typeable t, Typeable v, Alpha t, Alpha v) => Alpha (Telescope v t)

instance (Subst t1 t2) => Subst t1 (Telescope v t2)


empty :: Telescope v t
empty = Empty

singleton :: (Typeable t, Typeable v, Alpha t, Alpha v) => Name v -> t -> Telescope v t
singleton x xt = extend x xt empty

null :: Telescope v t -> Bool
null Empty = True
null _ = False

extend :: (Typeable t, Typeable v, Alpha t, Alpha v) => Name v -> t -> Telescope v t -> Telescope v t
extend x xt tl = SnocHyp (rebind tl (x, embed xt))

concat :: (Typeable t, Typeable v, Alpha t, Alpha v) => Telescope v t -> Telescope v t -> Telescope v t
concat tl1 Empty = tl1
concat tl1 (SnocHyp (unrebind -> (tl2, (x, unembed -> xt)))) = SnocHyp (rebind (concat tl1 tl2) (x, embed xt))

-- | Infix, flipped, uncurried version of extend
(@>) :: (Typeable t, Typeable v, Alpha t, Alpha v) => Telescope v t -> (Name v, t) -> Telescope v t
tl @> (x, xt) = extend x xt tl

find :: (Typeable t, Typeable v, Alpha t, Alpha v) => (t -> Bool) -> Telescope v t -> Maybe (Name v, t)
find _ Empty = Nothing
find p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p xt = Just (x, xt)
                                                        | otherwise = find p tl

{-# WARNING lookupKey "Revert back the comparison to regular equality" #-}
lookupKey :: (Typeable t, Typeable v, Alpha t, Alpha v) => Name v -> Telescope v t -> Maybe t
lookupKey _ Empty = Nothing
lookupKey n (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | (name2String x) == (name2String n) = Just xt
                                                             | otherwise = lookupKey n tl

anyKey :: (Typeable t, Typeable v, Alpha t, Alpha v) => (Name v -> t -> Bool) -> Telescope v t -> Bool
anyKey _ Empty = False
anyKey p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p x xt = True
                                                          | otherwise = anyKey p tl

map :: (Typeable a, Typeable b, Typeable v, Alpha a, Alpha b, Alpha v) => (a -> b) -> Telescope v a -> Telescope v b
map _ Empty = Empty
map f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = SnocHyp (rebind (map f tl) (x, embed $ f xt))

-- Note that Telescope v v is not an instance of Foldable, as we can only fold when the elements can be compared using α-equivalence
foldr :: (Typeable a, Typeable v, Alpha a, Alpha v) => (a -> b -> b) -> b -> Telescope v a -> b
foldr _ b Empty = b
foldr f b (SnocHyp (unrebind -> (tl, (_, unembed -> xt)))) = f xt (foldr f b tl)

foldl :: (Typeable a, Typeable v, Alpha a, Alpha v) => (b -> a -> b) -> b -> Telescope v a -> b
foldl _ b Empty = b
foldl f b (SnocHyp (unrebind -> (tl, (_, unembed -> xt)))) = foldl f (f b xt) tl

foldlM :: (Typeable a, Typeable v, Alpha a, Alpha v, Monad m) => (b -> a -> m b) -> b -> Telescope v a -> m b
foldlM _ b Empty = return b
foldlM f b (SnocHyp (unrebind -> (tl, (_, unembed -> xt)))) = do
    b' <- f b xt
    foldlM f b' tl

foldrWithKey :: (Typeable a, Typeable v, Alpha a, Alpha v) => (Name v -> a -> b -> b) -> b -> Telescope v a -> b
foldrWithKey _ b Empty = b
foldrWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = f x xt (foldrWithKey f b tl)

foldlWithKey :: (Typeable a, Typeable v, Alpha a, Alpha v) => (b -> Name v -> a -> b) -> b -> Telescope v a -> b
foldlWithKey _ b Empty = b
foldlWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = foldlWithKey f (f b x xt) tl

foldlMWithKey :: (Typeable a, Typeable v, Alpha a, Alpha v, Monad m) => (b -> Name v -> a -> m b) -> b -> Telescope v a -> m b
foldlMWithKey _ b Empty = return b
foldlMWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = do
    b' <- f b x xt
    foldlMWithKey f b' tl

foldrMWithKey :: (Typeable a, Typeable v, Alpha a, Alpha v, Monad m) => (Name v -> a -> b -> m b) -> b -> Telescope v a -> m b
foldrMWithKey _ b Empty = return b
foldrMWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = do
    b' <- foldrMWithKey f b tl
    f x xt b'

traverse :: (Typeable a, Typeable b, Typeable v, Alpha a, Alpha b, Alpha v, Applicative f) => (a -> f b) -> Telescope v a -> f (Telescope v b)
traverse _ Empty = pure Empty
traverse f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = (\xt' tl' -> SnocHyp (rebind tl' (x, embed xt'))) <$> f xt <*> traverse f tl

traverseWithKey :: (Typeable a, Typeable b, Typeable v, Alpha a, Alpha b, Alpha v, Applicative f) => (Name v -> a -> f b) -> Telescope v a -> f (Telescope v b)
traverseWithKey _ Empty = pure Empty
traverseWithKey f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = (\xt' tl' -> SnocHyp (rebind tl' (x, embed xt'))) <$> f x xt <*> traverseWithKey f tl 

toList :: (Typeable a, Typeable v, Alpha a, Alpha v) => Telescope v a -> [(Name v, a)]
toList = foldlWithKey (\xs x xt -> (x,xt):xs) []

unzip :: (Typeable a, Typeable b, Typeable v, Alpha a, Alpha b, Alpha v) => Telescope v (a,b) -> (Telescope v a, Telescope v b)
unzip = foldrWithKey (\x (a, b) (ta, tb) -> (extend x a ta, extend x b tb)) (empty, empty)
