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
    | Pair Term Term
    | Prod Term Term
    | Equals Term Term Term
    deriving (Show, Generic, Typeable)

-- | Extracts need to be able to handle non-standard substitutions
newtype Extract = Extract { unExtract :: Term }
    deriving (Show, Generic, Typeable)

type MetaVar = Name Extract

-- | Used to implement the delayed binding behavior of a metavariable
data DelayedBind
    = Close AlphaCtx NamePatFind
    | Open AlphaCtx NthPatFind

instance Show DelayedBind where
    show (Close _ _) = "<close>"
    show (Open _ _) = "<open>"

newtype DelayedBinds = DelayedBinds { unDelayedBinds :: [DelayedBind] }
    deriving (Show)

instance Alpha DelayedBinds where
    aeq' _ _ _ = True
    fvAny' _ _ = pure
    close ctx np (DelayedBinds bs) = DelayedBinds $ Close ctx np:bs
    open ctx np (DelayedBinds bs) = DelayedBinds $ Open ctx np:bs
    isPat _ = inconsistentDisjointSet
    isTerm _ = mempty
    namePatFind  _ = NamePatFind $ const $ Left 0
    nthPatFind _ = NthPatFind Left
    swaps' _ _ = id
    lfreshen' _ i cont = cont i mempty
    freshen' _ i = return (i, mempty)
    acompare' _ _ _ = EQ

data MetaSubst = MetaSubst 
    { metaSubst :: [(Var, Term)]
    , closedVars :: DelayedBinds
    }
    deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Extract
instance Alpha MetaSubst

instance Subst Term Term where
    isvar (Var x) = Just $ SubstName x
    isvar _ = Nothing

instance Subst Term Extract where
    subst _ _ = id
    substs _ = id
instance Subst Extract Extract

instance Subst Extract Term where
    -- When we substitute a metavar into a term, we apply all of the substitutions we've built up
    isCoerceVar (Hole ms x) = Just $ SubstCoerce x (Just . applyMetaSubst ms x)
    isCoerceVar _ = Nothing

instance Subst Term MetaSubst where
    subst x t ms = ms { metaSubst = subst x t (metaSubst ms) }
    substs ts ms = ms { metaSubst = substs ts (metaSubst ms) }
instance Subst Extract MetaSubst where
    subst x e ms = ms { metaSubst = subst x e (metaSubst ms) }
    substs es ms = ms { metaSubst = substs es (metaSubst ms) }

applyMetaSubst :: MetaSubst -> MetaVar -> Extract -> Term
applyMetaSubst (MetaSubst ms cl) x e = 
    let t = substs ms $ unExtract e
        applyBind b t = case b of
            Close ctx np -> close ctx np t
            Open ctx np -> open ctx np t
    in foldr applyBind t $ unDelayedBinds cl

-- | Creates a hole with no meta-substitutions
hole :: MetaVar -> Term
hole = Hole (MetaSubst [] (DelayedBinds []))

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcard :: String
wildcard = "_"