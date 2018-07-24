module MuPRL.Refine.TacticSpec where

import Test.Hspec

import Unbound.Generics.LocallyNameless

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.IO.Class

import MuPRL.Core.Term

import MuPRL.Parser.Vernacular (vernacular)
import MuPRL.Parser.Stack (runParser)
import MuPRL.Core.Unbound.MonadName (runNameMT)
import MuPRL.Vernacular.Eval

import MuPRL.Repl.MonadRepl
import MuPRL.Repl.Repl

import MuPRL.PrettyPrint

testTactic :: FilePath -> Term -> Spec
testTactic path t = 
    describe path $
        it ("should extract " ++ (show $ pretty t)) $ do
            res <- runRepl $ do
                contents <- liftIO $ T.readFile path
                p <- hoistError $ runParser vernacular contents
                extracts <- traverse (\v -> hoistError =<< runNameMT (evalVernacular v)) p
                return $ unExtract $ head extracts
            res `shouldSatisfy` (aeq t)
        -- let res = runRepl $ do
                -- contents <- liftIO $ T.readFile path
                -- p <- hoistError $ runParser vernacular contents
                -- extracts <- traverse (\v -> hoistError =<< runNameMT (evalVernacular v)) p
                -- return $ unExtract $ head extracts
        -- in describe path $ 
        --     it "" $
        --         (res `shouldSatisfy` (aeq t))

spec :: Spec
spec = describe "MuPRL.Refine.TacticSpec" $ do
    testTactic "samples/id.mprl" (lambda (s2n "a") (lambda (s2n "x") $ Var (s2n "x")))