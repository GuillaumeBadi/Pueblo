
module TypeCheck where

import Types

-- returns True if the parameter is a Constant
isConstant :: Expression -> Bool
isConstant (Constant _) = True
isConstant _            = False

-- returns True if the parameter is a Variable
isVar :: Expression -> Bool
isVar (Var _)   = True
isVar _         = False

-- returns True if the parameter is a Constraint
isConstraint :: Expression -> Bool
isConstraint (_ :- _) = True
isConstraint _       = False

-- -- returns True if the parameter is a Filter
-- isFilter :: Expression -> Bool
-- isFilter (_ :<< _) = True
-- isFilter _         = False

-- returns True if the parameter is a List
isList :: Expression -> Bool
isList (List _) = True
isList _        = False
