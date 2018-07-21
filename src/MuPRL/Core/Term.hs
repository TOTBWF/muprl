module MuPRL.Core.Term where


import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.Core.Unbound
import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl

import Debug.Trace

type Var = Name Term

data Term
    = Var Var
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

newtype MetaSubst = MetaSubst { metaSubst :: [(Var, Term)] }
    deriving (Show, Generic, Typeable, Monoid)

instance Alpha Term
instance Alpha Extract
instance Alpha MetaSubst

instance Subst Term Term where
    isvar (Var x) = Just $ SubstName x
    -- When we see a hole, we want to insert the substitution into the list
    isvar _ = Nothing

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


instance Subst Term Extract where
    subst _ _ = id
    substs _ = id
instance Subst Extract Extract

instance Subst Extract Term where
    -- When we substitute a metavar into a term, we apply all of the substitutions we've built up
    isCoerceVar (Hole ms x) = Just $ SubstCoerce x (Just . applyMetaSubst ms)
    isCoerceVar _ = Nothing

instance Subst Term MetaSubst
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

wildcard :: String
wildcard = "_"