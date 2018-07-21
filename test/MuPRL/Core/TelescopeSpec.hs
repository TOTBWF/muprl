module MuPRL.Core.TelescopeSpec where

import Test.Hspec

import Unbound.Generics.LocallyNameless

import MuPRL.Core.Term
import MuPRL.Core.Telescope (Telescope, (@>))
import qualified MuPRL.Core.Telescope as Tl

tl1 :: Telescope Term Term
tl1 = Tl.empty @> (string2Name "a", Universe 0) @> (string2Name "x", Var $ string2Name "a")

spec :: Spec
spec = describe "MuPRL.Core.Telescope" $ do
    toListSpec
    lookupKeySpec
    foldrSpec

toListSpec :: Spec
toListSpec = describe "toList" $ 
    it "turns a telescope into a list" $ 
        (Tl.toList tl1) `shouldSatisfy` (aeq [(string2Name "a", Universe 0), (string2Name "x", Var $ string2Name "a")])

lookupKeySpec :: Spec
lookupKeySpec = describe "lookupKey" $
    it "looks up a key in a telescope" $
        (Tl.lookupKey (s2n "a") tl1) `shouldSatisfy` (\(Just t) -> t `aeq` (Universe 0))

foldrSpec :: Spec
foldrSpec = describe "foldr" $
    it "performs a right fold on a telescope" $
        (Tl.foldr (:) [] tl1) `shouldSatisfy` (aeq [Var $ string2Name "a", Universe 0])
        


-- withTelescopeSpec :: Spec
-- withTelescopeSpec = describe "withTelescope" $
--     it "binds all variables in the term" $
        -- (Tl.withTelescope tl1 (var $ string2Name "x"))