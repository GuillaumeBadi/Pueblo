
module Has where

import Pueblo

has :: Expression -> Expression -> Expression
has a b = List [ Constant "has", a, b ]

phone :: Expression -> Expression -> Expression
phone a b = List [ Constant "phone", a, b ]

x = Var "x"
y = Var "y"

guillaume = Constant "guillaume"
nexus     = Constant "nexus"
google    = Constant "google"

facts =
  [ has guillaume (phone nexus google) ]

e = case solve [] facts (has guillaume (phone x y)) of
  Nothing       -> print "Nothing"
  Just bindings -> print $ bindingsToString bindings
