module Domain where

import Data.List

-- While-Cons Syntax

type Var = String

data C = Cskip 
        | Cassign Var E 
        | Cseq C C 
        | Cfor E C 
        | Cif E C C 
        | Cwhile E C
        deriving Show

data E = Ezero 
        | Esucc E 
        | Epred E 
        | Eif E E E 
        | Evar Var
        | Etrue 
        | Efalse 
        | Elt E E 
        | Eeq E E 
        | Enot E
        | Econs E E 
        | Ehd E 
        | Etl E
        deriving Show

-- Semantic Domains

data V = IntValue {getInt :: Int} 
        | BoolValue {getBool :: Bool}
        deriving (Ord, Eq)

type Domain = [V]

type State = Var -> Domain

-- Instances
instance Show V where
    showsPrec p (BoolValue True) = ("true" ++)
    showsPrec p (BoolValue False) = ("false" ++)
    showsPrec p (IntValue n) = ((show n) ++)

    showList [m] = shows m
    showList (m:ms) = shows m . (" : " ++) . showList ms

