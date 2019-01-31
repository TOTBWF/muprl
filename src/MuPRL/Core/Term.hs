-- |
-- Module      :  MuPRL.Core.Term
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
module MuPRL.Core.Term where

import GHC.Generics
import Data.Typeable (Typeable)

import Unbound.Generics.LocallyNameless
import MuPRL.Core.Telescope (Telescope)
import qualified MuPRL.Core.Telescope as Tl

import Debug.Trace
type Var = Name Term
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

-- | Extracts need to be able to handle non-standard substitutions
newtype Extract = Extract { unExtract :: Term }
    deriving (Show, Generic, Typeable)

type MetaVar = Name Extract

instance Alpha Term
instance Alpha Extract

instance Subst Term Term where
    isvar (Var x) = Just $ SubstName x
    -- When we see a hole, we want to insert the substitution into the list
    isvar _ = Nothing

instance Subst Term Extract where
    subst _ _ = id
    substs _ = id

lambda :: Var -> Term -> Term
lambda x body = Lam (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcard :: String
wildcard = "_"
