module MuPRL.Environment where

import MuPRL.Syntax

data Env = Env 
    { envProofState :: [(Var, Term)]
    , envLevel :: Int
    }

emptyEnv :: Env
emptyEnv = Env
    { envProofState = []
    , envLevel = 0
    }

class HasProofState env where
    getProofState :: env -> [(Var, Term)]

instance HasProofState Env where
    getProofState = envProofState