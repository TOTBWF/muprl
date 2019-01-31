module MuPRL.PrettyPrint
    ( module P
    , PrettyM (..)
    )
where

import Data.Typeable

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl

import Data.Text.Prettyprint.Doc as P


-- Because of the way we handle bindings, we need to be able to ensure that all
-- of the variables are "locally fresh" when we perform the unbinding
-- This means that we need to wrap the entire prettyprint computation
-- inside of a local freshness monad. We then run that computation
-- when we no longer need local freshness
class PrettyM a where
    prettyM :: a -> NameM (Doc ann)

instance Pretty (Name t) where
    pretty = pretty . show

instance (PrettyM t, Typeable t, Typeable v, Alpha t, Alpha v) => PrettyM (Telescope v t) where
    prettyM tl = do
        ptl <- traverse (\(x,xt) -> fmap (\pxt -> parens $ pretty x <> pretty ":" <> pxt) $ prettyM xt) $ Tl.toList tl
        return $ hsep (punctuate comma ptl)

instance (PrettyM t, Typeable t, Typeable v, Alpha t, Alpha v) => Pretty (Telescope v t) where
    pretty = runNameM . prettyM

instance Pretty Term where
    pretty = runNameM . prettyM

instance Pretty Extract where
    pretty = runNameM . prettyM

instance PrettyM Extract where
    prettyM = prettyM . unExtract

instance PrettyM Term where
    prettyM (Var x) = return $ pretty x
    prettyM (Hole h) = pure $ pretty h
    prettyM Void = return $ pretty "void"
    prettyM Axiom = return $ pretty "axiom"
    prettyM (Universe k) = return $ pretty "universe" <+> pretty k
    prettyM (Lam bnd) = lunbind bnd $ \(x,b) -> (\b -> pretty "\\" <> pretty x <> dot <+> b) <$> prettyM b  
    prettyM (Pi bnd) = lunbind bnd $ \(x, t, b) -> do
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
