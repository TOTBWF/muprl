module MuPRL.Core.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.PrettyPrint

type Var = Name Term

type MetaVar = Name Term

type Extract = Term

data Term
    = Var Var
    | Void
    | Axiom
    | Universe Int
    | Lam (Bind Var Term) 
    | Pi (Bind (Var, Embed Term) Term)
    | App Term Term
    | Equals Term Term Term
    deriving (Show, Generic, Typeable)

instance Alpha Term

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _ = Nothing

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcardName :: (Fresh m) => m MetaVar
wildcardName = (fresh $ string2Name "_")

metavar :: (Fresh m) => m MetaVar
metavar = (fresh $ string2Name "?_")

fvSet :: (Alpha a, Typeable a) => a -> Set (Name a)
fvSet = Set.fromList . toListOf fv

{- Pretty Printing -}

instance Pretty (Name a) where
    pretty = pretty . name2String

instance Pretty Term where
    pretty = runLFreshM . prettyM

instance PrettyM Term where
    prettyM (Var x) = return $ pretty x
    -- prettyM (Hole x) = return $ pretty "?" <> pretty x
    prettyM Void = return $ pretty "void"
    prettyM Axiom = return $ pretty "axiom"
    prettyM (Universe k) = return $ pretty "universe" <+> pretty k
    prettyM (Lam bnd) = lunbind bnd $ \(x,b) -> (\b -> pretty "\\" <> pretty x <> dot <+> b) <$> prettyM b  
    prettyM (Pi bnd) = lunbind bnd $ \((x,unembed -> t), b) -> do
        pt <- prettyM t
        pb <- prettyM b
        return $ parens (pretty x <> pretty ":" <> pt) <+> pretty "->" <+> pb
    prettyM (App f a) = do
        pf <- prettyM f
        pa <- prettyM a
        return $ pf <+> pa
    prettyM (Equals e1 e2 t) = do
        pe1 <- prettyM e1
        pe2 <- prettyM e2
        pt <- prettyM t
        return $ pe1 <+> pretty "=" <+> pe2 <+> pretty "in" <+> pt
