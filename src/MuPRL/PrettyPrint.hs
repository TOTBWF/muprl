module MuPRL.PrettyPrint where

import Text.PrettyPrint
import Unbound.Generics.LocallyNameless
import Data.Typeable (Typeable)
import Control.Monad

import MuPRL.Syntax
import MuPRL.LCF
import MuPRL.Rules

class Pretty p where
    ppr :: p -> LFreshM Doc

instance Pretty Int where
    ppr = return . integer. toInteger

instance Typeable a => Pretty (Name a) where
    ppr = return . text . name2String

instance Pretty Term where
    ppr (Var x) = ppr x
    ppr Void = return $ text "void"
    ppr Unit = return $ text "unit"
    ppr Nil = return $ text "nil"
    ppr (Universe k) = do
        pk <- ppr k
        return $ text "universe" <+> pk
    ppr (App f a) = do
        pf <- ppr f
        pa <- ppr a
        return $ pf <+> pa
    ppr (Lambda bnd) = 
            lunbind bnd $ \(x,b) -> do
                px <- ppr x
                pb <- ppr b
                return $ text "\\" <> px <> text "." <+> pb 
    ppr (Pi bnd) =
            lunbind bnd $ \((x,t),b) -> do
                px <- ppr x
                pt <- ppr (unembed t)
                pb <- ppr b
                return $ parens (px <> text ":" <> pt) <+> text "->" <+> pb
    ppr (Equals t1 t2 typ) = do
        pt1 <- ppr t1
        pt2 <- ppr t2
        ptyp <- ppr typ
        return $ pt1 <+> text "=" <+> pt2 <+> text "in" <+> ptyp
    ppr Axiom = return $ text "axiom"


instance Pretty Judgement where
    ppr (ctx :>> t) = do
        pctx <- foldM (\p (x,t') -> do
                px <- ppr x
                pt' <- ppr t'
                return $ px <> text ":" <> pt' <> text "," <+> p) (text "") ctx
        pt <- ppr t
        return $ pctx <> text "H >>" <+> pt

pp :: (Pretty a) => a -> String
pp = render . runLFreshM . ppr