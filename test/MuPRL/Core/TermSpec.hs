module MuPRL.Core.TermSpec where

import Test.Hspec

import Unbound.Generics.LocallyNameless

import MuPRL.Core.Term

spec :: Spec
spec = describe "MuPRL.Core.Term" $ do
    holeSpec

holeSpec :: Spec
holeSpec = describe "hole substitution" $ do
    let mv1 = (s2n "_" :: MetaVar)
    let mv2 = (s2n "_1" :: MetaVar)
    let h1 = hole mv1
    let h2 = Hole (MetaSubst [(s2n "a", Var $ s2n "x")]) mv1
    let h3 = Hole (MetaSubst [(s2n "a", hole mv1)]) mv2
    it "should fill in metavariables" $
        (subst mv1 (Extract Void) h1) `shouldSatisfy` (aeq Void)
    it "should apply metasubstitutions" $
        (subst mv1 (Extract $ Var $ s2n "a") h2) `shouldSatisfy` (aeq $ Var $ s2n "x")
    it "should apply to nested metasubsts" $
        (subst mv1 (Extract $ Var $ s2n "c") h3) `shouldSatisfy` (aeq $ Hole (MetaSubst [(s2n "a", Var $ s2n "c")]) mv2)
