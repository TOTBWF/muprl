module MuPRL.Core.Unbound
    ( module Subst
    , Alpha(..), aeq
    , Bind, bind
    , Name
    , Embed, embed, unembed
    , name2String, string2Name, s2n
    , fvSet)
where

import Data.Typeable

import Data.Set (Set)
import qualified Data.Set as Set

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Unbound.Generics.LocallyNameless.Subst as Subst

fvSet :: (Alpha a, Typeable a) => a -> Set (Name a)
fvSet = Set.fromList . toListOf fv