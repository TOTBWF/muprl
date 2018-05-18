module MuPRL.Core.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import GHC.Generics
import Data.Typeable (Typeable)

import MuPRL.PrettyPrint

type Var = Name Term

type MetaVar = Name Term

type Extract = Term

data Term
    = Var Var
    | Hole MetaVar
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
    isvar (Hole x) = Just (SubstName x)
    isvar _ = Nothing

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcardName :: (Fresh m) => m MetaVar
wildcardName = (fresh $ string2Name "_")

metavar :: (Fresh m) => m MetaVar
metavar = (fresh $ string2Name "_")

{- Pretty Printing -}

instance Pretty (Name a) where
    pretty = pretty . name2String

instance Pretty Term where
    pretty = runLFreshM . prettyFresh

instance PrettyFresh Term where
    prettyFresh (Var x) = return $ pretty x
    prettyFresh (Hole x) = return $ pretty "?" <> pretty x
    prettyFresh Void = return $ pretty "void"
    prettyFresh Axiom = return $ pretty "axiom"
    prettyFresh (Universe k) = return $ pretty "universe" <+> pretty k
    prettyFresh (Lam bnd) = lunbind bnd $ \(x,b) -> (\b -> pretty "\\" <> pretty x <> dot <+> b) <$> prettyFresh b  
    prettyFresh (Pi bnd) = lunbind bnd $ \((x,unembed -> t), b) -> do
        pt <- prettyFresh t
        pb <- prettyFresh b
        return $ parens (pretty x <> pretty ":" <> pt) <+> pretty "->" <+> pb
    prettyFresh (App f a) = do
        pf <- prettyFresh f
        pa <- prettyFresh a
        return $ pf <+> pa
    prettyFresh (Equals e1 e2 t) = do
        pe1 <- prettyFresh e1
        pe2 <- prettyFresh e2
        pt <- prettyFresh t
        return $ pe1 <+> pretty "=" <+> pe2 <+> pretty "in" <+> pt
