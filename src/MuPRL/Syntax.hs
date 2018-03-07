{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
 
module MuPRL.Syntax where

import Control.Monad.Reader
import Control.Monad.Except
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import GHC.Generics
import Data.Typeable (Typeable)


import MuPRL.Refinement

{-
TODO: Explain Canonical terms
-}

type Var = Name Term

data Term
    = Var Var                       -- A Variable 
    | Void                          -- Void Type
    | Unit                          -- Unit type
    | Universe Int                  -- A Type Universe 
    | App Term Term                 -- Function Application
    | Lambda (Bind Var Term)        -- Lambda Abstraction 
    | Pi (Bind (Var, Embed Term)  Term)            -- Dependent Function Type
    | Equals Term Term Term         -- Equality (a = b in A)
    | Axiom                         -- The only inhabitant of the equality type
    deriving (Show, Generic, Typeable)

instance Alpha Term

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _ = Nothing

-- Smart Contstructors

lambda :: Var -> Term -> Term    
lambda x body = Lambda (bind x body)

pi :: Var -> Term -> Term -> Term
pi x typ body = Pi (bind (x, embed typ) body)

wildcardName :: Var
wildcardName = string2Name "_"

-- Some utility functions we need to determine if a term is closed
freeVars :: Term -> [Var]
freeVars = toListOf fv

closed :: Term -> Bool
closed = null . freeVars

{-
TODO: Explan evaluation
Note that evaluation is lazy!
-}

type TC = FreshM

evaluate :: Term -> TC Term
-- If a term is canonical, we do not evaluate any further
evaluate (Var x) = return $ Var x
evaluate (Void) = return $ Void
evaluate (Unit) = return $ Unit
evaluate (Universe k) = return $ Universe k
evaluate (Lambda bnd) = return $ Lambda bnd
evaluate (Pi bnd) = return $ Pi bnd
evaluate (Equals a b typ) = return $ Equals a b typ
evaluate (Axiom) = return $ Axiom
evaluate (App (Lambda bnd) a) = do
    (x, b) <- unbind bnd
    return $ subst x a b


{-
To define a type we specify a collection of canonical terms which are the canonical elements of the type,
and we define an equality relation declaring when two canonical terms denote the same abstract object

Example 1: 0,1,2,3... are the canonical terms that are elements of `Nat`
The equality relation of Nat is just the equality relation on natural numbers.
Keep in mind that this equality applies after reduction, so `10 + 7 = 17` and `(\x. x + 1)(0) = 1`

Using this principle, we can start to define propositions.
A term `a` is an element of a type `A` if and only if `a = a` under the equivalence relation of `A`
-}

{-
To prove a judgement, we need to show that the type is inhabited. 
This can be done by either providing direct evidence, or by further refinement.
-}