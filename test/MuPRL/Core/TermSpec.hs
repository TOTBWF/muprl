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
    let x = s2n "x" :: Var
    let f = s2n "f" :: Var
    let a = s2n "a" :: Var
    let h1 = hole mv1
    let h2 = Hole (MetaSubst [(x, Var a)]) mv1
    let h3 = Hole (MetaSubst [(x, hole mv1)]) mv2
    let h4 = Hole (MetaSubst {metaSubst = [(a ,App (Var $ f) (Hole (MetaSubst {metaSubst = []}) mv1))]}) mv2
    it "should fill in metavariables" $
        (subst mv1 (Extract Void) h1) `shouldSatisfy` (aeq Void)
    it "should apply metasubstitutions" $
        (subst mv1 (Extract $ Var x) h2) `shouldSatisfy` (aeq $ Var a)
    it "should apply to nested holes" $
        (subst mv1 (Extract $ Var f) h3) `shouldSatisfy` (aeq $ Hole (MetaSubst [(x, Var f)]) mv2)
    it "should apply to nested holes inside terms" $
        (substs [(mv2, Extract $ Var a), (mv1, Extract $ Var x)] h4) `shouldSatisfy` (aeq $ App (Var f) (Var x))
