module MuPRL.Core.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.PrettyPrint

import Debug.Trace

type Var = Name Term

data Term
    = Var Var
    -- | Hole MetaSubst MetaVar
    | Hole Hole
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

data Hole 
    = HoleVar [(Var, Term)] MetaVar
    | FilledHole Term
    deriving (Show, Generic, Typeable)

-- newtype MetaSubst = MetaSubst { metaSubst :: [(Var, Term)] }
--     deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Extract
instance Alpha Hole
-- instance Alpha MetaSubst

instance Subst Term Term where
    isvar (Var x) = Just $ SubstName x
    isvar _ = Nothing

instance Subst Term Hole where
    subst x t (HoleVar vs mx) = HoleVar ((x,t):subst x t vs) mx
    subst x t (FilledHole t') = FilledHole (subst x t t')

    substs ts (HoleVar vs mx) = HoleVar (foldr (\(x,t) vs' -> (x,t):subst x t vs') vs ts) mx
    substs ts (FilledHole t) = FilledHole (substs ts t)

instance Subst Extract Hole where
    subst mx e (HoleVar vs mx') | mx == mx' = FilledHole (substs vs $ unExtract e)
                                | otherwise = HoleVar (subst mx e vs) mx'
    subst mx e (FilledHole t) = FilledHole (subst mx e t)
-- instance Subst Term MetaSubst where
--     subst x t (MetaSubst vs) = MetaSubst ((x,t):subst x t vs)
--     substs ss (MetaSubst vs) = MetaSubst $ foldr (\(x,t) vs' -> ((x,t):subst x t vs')) vs ss

-- instance Subst Extract MetaSubst where
--     subst x t (MetaSU)


-- This could be wrong
instance Subst Term Extract
instance Subst Extract Extract

instance Subst Extract Term where
    -- -- When we substitute a metavar into a term, we apply all of the substitutions we've built up
    -- isCoerceVar (Hole vs x) = Just $ SubstCoerce x (Just . applyMetaSubst vs)
    -- isCoerceVar _ = Nothing

-- applyMetaSubst :: MetaSubst ->  Extract -> Term
-- applyMetaSubst (MetaSubst vs) e = substs vs $ unExtract e


-- | Creates a hole with no meta-substitutions
hole :: MetaVar -> Term
hole = Hole . HoleVar []

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcard :: (Fresh m) => m (Name t)
wildcard = (fresh $ string2Name "_")

metavar :: (Fresh m) => Name Term -> m MetaVar
metavar = fresh . string2Name . name2String

var :: (Fresh m) => MetaVar -> m Var
var = fresh . string2Name . name2String

fvSet :: (Alpha a, Typeable a) => a -> Set (Name a)
fvSet = Set.fromList . toListOf fv

{- Pretty Printing -}

instance Pretty (Name t) where
    pretty = pretty . show


instance {-# OVERLAPPING #-} Pretty MetaVar where
    pretty x = pretty "?" <> (pretty $ show x)

instance Pretty Term where
    pretty = runLFreshM . prettyM

instance Pretty Extract where
    pretty = runLFreshM . prettyM

instance PrettyM Extract where
    prettyM = prettyM . unExtract

instance PrettyM Hole where
    prettyM (HoleVar vs mx) = do
        pvs <- traverse (\(x,t) -> (\pt -> pt <> pretty "/" <> pretty x) <$> prettyM t) vs
        return $ pretty mx <> brackets (hsep $ punctuate comma pvs)
    prettyM (FilledHole t) = prettyM t

instance PrettyM Term where
    prettyM (Var x) = return $ pretty x
    prettyM (Hole h) = prettyM h
    -- prettyM (Hole (MetaSubst vs) x) = return $ pretty x <> (pretty vs)
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
