
module Types where

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map
import           Data.List

type Filter = Binding -> Bool

{- An Expression is either:
      a Constant with a name (a String) TODO: a constant can be any type
      a Variable with a name (a String)
      a List of expressions -}
data Expression
  -- A Constant is an atom whose value cannot change
  -- (Constant "romeo") is a fixed value
  = Constant String
  -- A Var is an empty space to fill
  -- Var "X" will be replaced with multiple values to test the newly created
  --   query
  | Var String
  -- This is a Constraint. It is an expression e followed by a list of expressions
  --  that must be valid to make e valid
  -- Example: [ grandFather x y :- [father x z, father z y] ]
  --        = x is the grandFather of y if y is the father of z and z is the father of y
  | Expression :- [Expression]
  -- A List is basically a fact or a query
  -- List [ Constant "likes", Constant "romeo", Constant "juliet" ]
  --   = likes romeo juliet
  | List [Expression]
  -- This is a Filter function, that.. filters the resulting bindings
  -- The expression is solved, then filtered by each function in the list
  | Expression :<< [Filter]
  -- deriving (Ord)

infix 6 :-
infix 5 :<<

instance Eq Expression where
  e1 :<< f1 == e2 :<< f2 = True
  e1 == e2 = e1 == e2

instance Ord Expression where
  (Var e) `compare` (Var e') = e `compare` e'
  (e1 :<< f1) `compare` (e2 :<< f2) = e1 `compare` e2

instance Show Expression where
  show (Constant c) = show c
  show (Var s)      = (show s)
  show (c :- l)     = (show c) ++ " " ++ (show (List l))
  show (List l)     = intercalate " " $ map show l

type Constraint = Expression
type Variable   = Expression
type Const      = Expression
type Fact       = Expression

-- A binding is an associative maps:
-- [(x, romeo), (y, juliet)]
type Binding    = Map Expression Expression
