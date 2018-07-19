module MuPRL.PrettyPrint
    ( module P
    , PrettyM (..)
    )
where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Unbound.Generics.LocallyNameless.LFresh

import Data.Text.Prettyprint.Doc as P


-- Because of the way we handle bindings, we need to be able to ensure that all
-- of the variables are "locally fresh" when we perform the unbinding
-- This means that we need to wrap the entire prettyprint computation
-- inside of a local freshness monad. We then run that computation
-- when we no longer need local freshness
class PrettyM a where
    prettyM :: a -> LFreshM (Doc ann)

instance Pretty (Name t) where
    pretty = pretty . show