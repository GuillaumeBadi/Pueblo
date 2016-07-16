
module ToString where

import           Types

import           Data.List
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

-- To String Functions
-- TODO: create show instances for Expressions
expressionToString :: Expression -> String
expressionToString (Constant x) = x
expressionToString (Var x)      = x ++ "?"
expressionToString (List (Constant x : xs))
  = x ++ " " ++ unwords (map expressionToString xs)

constraintsToString :: Constraint -> String
constraintsToString (c :- l)
  = (expressionToString c) ++
    " :- " ++
    (intercalate ", " $ map expressionToString l)

bindingsToString :: [Binding] -> String
bindingsToString b
  = intercalate ", " $ map (\x -> "(" ++ bindingToString x ++ ")") b

bindingToString b
  = intercalate ", " $
    map (\((Var k), v) -> k ++ ": " ++ expressionToString v) (Map.assocs b)
