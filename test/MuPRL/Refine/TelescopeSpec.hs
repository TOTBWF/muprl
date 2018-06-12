module MuPRL.Refine.TelescopeSpec where

import Test.Hspec

import Unbound.Generics.LocallyNameless

import MuPRL.Core.Term
import MuPRL.Refine.Telescope (Telescope, (@>))
import qualified MuPRL.Refine.Telescope as Tl

tl1 :: Telescope Term
tl1 = Tl.empty @> (string2Name "a", Universe 0) @> (string2Name "x", Var $ string2Name "a")

spec :: Spec
spec = toListSpec

toListSpec :: Spec
toListSpec = describe "toList" $ 
    it "turns a telescope into a list" $ 
        (Tl.toList tl1) `shouldSatisfy` (aeq [(string2Name "a", Universe 0), (string2Name "x", Var $ string2Name "a")])

-- withTelescopeSpec :: Spec
-- withTelescopeSpec = describe "withTelescope" $
--     it "binds all variables in the term" $
        -- (Tl.withTelescope tl1 (var $ string2Name "x"))