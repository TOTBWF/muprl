module MuPRL.Core.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.PrettyPrint

import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl

import Debug.Trace

type Var = Name Term

data Term
    = Var Var
    | Nominal Nominal
    | Hole MetaSubst MetaVar 
    | Void
    | Axiom
    | Universe Int
    | Lam (Bind Var Term) 
    | Pi (Bind (Var, Embed Term) Term)
    | App Term Term
    | Equals Term Term Term
    deriving (Show, Generic, Typeable)

-- | Extracts need to be able to handle non-standard substitutions
newtype Extract = Extract { unExtract :: Term }
    deriving (Show, Generic, Typeable)

type MetaVar = Name Extract

-- | Nominals are used to represent the free variables of an extract
newtype Nom = Nom { unNom :: Term }
    deriving (Show, Generic, Typeable)

type Nominal = Name Nom

newtype MetaSubst = MetaSubst { metaSubst :: [(Nominal, Nom)] }
    deriving (Show, Generic, Typeable, Monoid)

instance Alpha Term
instance Alpha Extract
instance Alpha Nom
instance Alpha MetaSubst

instance Subst Term Term where
    isvar (Var x) = Just $ SubstName x
    -- When we see a hole, we want to insert the substitution into the list
    isvar _ = Nothing

instance Subst Nom Term where
    isCoerceVar (Nominal n) = Just $ SubstCoerce n (Just . unNom)
    isCoerceVar _ = Nothing

    -- subst x e (Hole (MetaSubst ms) v) = Hole (MetaSubst $ (x,e):ms) v

-- instance Subst Term Hole where
--     subst x t (HoleVar vs mx) = HoleVar ((x,t):subst x t vs) mx
--     subst x t (FilledHole t') = FilledHole (subst x t t')

--     substs ts (HoleVar vs mx) = HoleVar (foldr (\(x,t) vs' -> (x,t):subst x t vs') vs ts) mx
--     substs ts (FilledHole t) = FilledHole (substs ts t)

-- instance Subst Extract Hole where
--     subst mx e (HoleVar vs mx') | mx == mx' = FilledHole (substs vs $ unExtract e)
--                                 | otherwise = HoleVar (subst mx e vs) mx'
--     subst mx e (FilledHole t) = FilledHole (subst mx e t)
-- instance Subst Term MetaSubst where
--     subst x t (MetaSubst vs) = MetaSubst ((x,t):subst x t vs)
--     substs ss (MetaSubst vs) = MetaSubst $ foldr (\(x,t) vs' -> ((x,t):subst x t vs')) vs ss


-- This could be wrong
instance Subst Term Nom
instance Subst Extract Nom
instance Subst Nom Nom

instance Subst Term Extract where
    subst _ _ = id
    substs _ = id
instance Subst Nom Extract where
    subst _ _ = id
    substs _ = id
instance Subst Extract Extract

instance Subst Extract Term where
    -- When we substitute a metavar into a term, we apply all of the substitutions we've built up
    isCoerceVar (Hole ms x) = Just $ SubstCoerce x (Just . applyMetaSubst ms)
    isCoerceVar _ = Nothing

instance Subst Term MetaSubst
instance Subst Nom MetaSubst where
    subst x n (MetaSubst ms) = (MetaSubst $ (x,n):ms)
    substs xs (MetaSubst ms) = (MetaSubst $ xs ++ ms)
instance Subst Extract MetaSubst

applyMetaSubst :: MetaSubst -> Extract -> Term
applyMetaSubst (MetaSubst ms) e = substs ms $ unExtract e


-- | Creates a hole with no meta-substitutions
hole :: MetaVar -> Term
hole = Hole (MetaSubst [])

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcard :: (Typeable t) => (LFresh m) => m (Name t)
wildcard = (lfresh $ string2Name "_")

nominal :: (LFresh m) => Name Term -> m Nominal
nominal = lfresh . string2Name . name2String

metavar :: (LFresh m) => Name Term -> m MetaVar
metavar = lfresh . string2Name . name2String

var :: (LFresh m) => MetaVar -> m Var
var = lfresh . string2Name . name2String

fvSet :: (Alpha a, Typeable a) => a -> Set (Name a)
fvSet = Set.fromList . toListOf fv

{- Pretty Printing -}
instance {-# OVERLAPPING #-} Pretty Nominal where
    pretty x = pretty "_" <> (pretty $ show x)

instance {-# OVERLAPPING #-} Pretty MetaVar where
    pretty x = pretty "?" <> (pretty $ show x)

instance Pretty Term where
    pretty = runLFreshM . prettyM

instance Pretty Nom where
    pretty = runLFreshM . prettyM

instance PrettyM Nom where
    prettyM = prettyM . unNom

instance Pretty Extract where
    pretty = runLFreshM . prettyM

instance PrettyM Extract where
    prettyM = prettyM . unExtract

instance PrettyM MetaSubst where
    prettyM (MetaSubst ms) = 
        let pp (x,t) = do
                pt <- prettyM t
                return $ pt <> pretty "/" <> pretty x
        in (brackets . hsep . punctuate comma) <$> traverse pp ms

instance PrettyM Term where
    prettyM (Var x) = return $ pretty x
    prettyM (Nominal n) = return $ pretty n
    prettyM (Hole ms h) = (<>) <$> prettyM ms <*> (pure $ pretty h)
    prettyM Void = return $ pretty "void"
    prettyM Axiom = return $ pretty "axiom"
    prettyM (Universe k) = return $ pretty "universe" <+> pretty k
    prettyM (Lam bnd) = lunbind bnd $ \(x,b) -> (\b -> pretty "\\" <> pretty x <> dot <+> b) <$> prettyM b  
    prettyM (Pi bnd) = lunbind bnd $ \((x,unembed -> t), b) -> do
        pt <- prettyM t
        pb <- prettyM b
        let pa = if (name2String x) == "_" 
            then pt 
            else parens (pretty x <> pretty ":" <> pt)
        return $ pa <+> pretty "->" <+> pb
    prettyM (App f a) = do
        pf <- prettyM f
        pa <- prettyM a
        return $ parens (pf <+> pa)
    prettyM (Equals e1 e2 t) = do
        pe1 <- prettyM e1
        pe2 <- prettyM e2
        pt <- prettyM t
        return $ pe1 <+> pretty "=" <+> pe2 <+> pretty "in" <+> pt
