
module Substitue where

import           Types
import           TypeCheck

import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

{- Takes a Map such as [(X, Constant value)] and an expression
   and returns the expression with variables
   replaced by elements of the Map -}
substitute :: Binding -> Expression -> Expression
substitute m c | isConstant c = c
substitute m (List l)         = List $ map (substitute m) l
substitute m v@(Var _)        = fromMaybe v (Map.lookup v m)

-- Substitute every given expression with a given binding
substituteAll :: [Expression] -> Binding -> [Expression]
substituteAll l b = map (substitute b) l
