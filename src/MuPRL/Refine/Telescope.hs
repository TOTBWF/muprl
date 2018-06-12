module MuPRL.Refine.Telescope where

import Prelude hiding (foldr, foldl, map, concat, traverse)

import qualified Prelude as P

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

instance (Subst Term t) => Subst Term (Telescope t)

instance (PrettyM t, Typeable t, Alpha t) => PrettyM (Telescope t) where
    prettyM tl = do
        ptl <- P.traverse (\(x,xt) -> fmap (\pxt -> pretty x <> pretty ":" <> pxt) $ prettyM xt) $ toList tl
        return $ hsep (punctuate comma ptl)

instance (PrettyM t, Typeable t, Alpha t) => Pretty (Telescope t) where
    pretty = runLFreshM . prettyM

empty :: Telescope t
empty = Empty

singleton :: (Typeable t, Alpha t) => Name Term -> t -> Telescope t
singleton x xt = extend x xt empty

null :: Telescope t -> Bool
null Empty = True
null _ = False

extend :: (Typeable t, Alpha t) => Name Term -> t -> Telescope t -> Telescope t
extend x xt tl = SnocHyp (rebind tl (x, embed xt))

concat :: (Typeable t, Alpha t) => Telescope t -> Telescope t -> Telescope t
concat tl1 Empty = tl1
concat tl1 (SnocHyp (unrebind -> (tl2, (x, unembed -> xt)))) = SnocHyp (rebind (concat tl1 tl2) (x, embed xt))

-- | Infix, flipped, uncurried version of extend extend
(@>) :: (Typeable t, Alpha t) => Telescope t -> (Name Term, t) -> Telescope t
tl @> (x, xt) = extend x xt tl

find :: (Typeable t, Alpha t) => (t -> Bool) -> Telescope t -> Maybe (Name Term, t)
find _ Empty = Nothing
find p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p xt = Just (x, xt)
                                                        | otherwise = find p tl

findKey :: (Typeable t, Alpha t) => Name Term -> Telescope t -> Maybe t
findKey _ Empty = Nothing
findKey n (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | x == n = Just xt
                                                           | otherwise = findKey n tl

anyKey :: (Typeable t, Alpha t) => (Name Term -> t -> Bool) -> Telescope t -> Bool
anyKey _ Empty = False
anyKey p (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) | p x xt = True
                                                          | otherwise = anyKey p tl

map :: (Typeable a, Typeable b, Alpha a, Alpha b) => (a -> b) -> Telescope a -> Telescope b
map _ Empty = Empty
map f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = SnocHyp (rebind (map f tl) (x, embed $ f xt))

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

foldlMWithKey :: (Typeable a, Alpha a, Monad m) => (b -> Name Term -> a -> m b) -> b -> Telescope a -> m b
foldlMWithKey _ b Empty = return b
foldlMWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = do
    b' <- f b x xt
    foldlMWithKey f b' tl

foldrMWithKey :: (Typeable a, Alpha a, Monad m) => (Name Term -> a -> b -> m b) -> b -> Telescope a -> m b
foldrMWithKey _ b Empty = return b
foldrMWithKey f b (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = do
    b' <- foldrMWithKey f b tl
    f x xt b'

traverse :: (Typeable a, Typeable b, Alpha a, Alpha b, Applicative f) => (a -> f b) -> Telescope a -> f (Telescope b)
traverse _ Empty = pure Empty
traverse f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = (\xt' tl' -> SnocHyp (rebind tl' (x, embed xt'))) <$> f xt <*> traverse f tl

traverseWithKey :: (Typeable a, Typeable b, Alpha a, Alpha b, Applicative f) => (Name Term -> a -> f b) -> Telescope a -> f (Telescope b)
traverseWithKey _ Empty = pure Empty
traverseWithKey f (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) = (\xt' tl' -> SnocHyp (rebind tl' (x, embed xt'))) <$> f x xt <*> traverseWithKey f tl 

toList :: (Typeable a, Alpha a) => Telescope a -> [(Name Term, a)]
toList = foldlWithKey (\xs x xt -> (x,xt):xs) []

unzip :: (Typeable a, Typeable b, Alpha a, Alpha b) => Telescope (a,b) -> (Telescope a, Telescope b)
unzip = foldrWithKey (\x (a, b) (ta, tb) -> (extend x a ta, extend x b tb)) (empty, empty)


{-# WARNING withTelescope "This may not be correct, use with caution" #-}
withTelescope :: (Subst Term t) => Telescope Term -> t -> t
withTelescope Empty t = t
withTelescope (SnocHyp (unrebind -> (tl, (x, unembed -> xt)))) t = 
    let t' = withTelescope tl t
    in subst x xt t'