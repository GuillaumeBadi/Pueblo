
module Family where

import          Pueblo

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

startWith v letter binding
  = case Map.lookup v binding of
      Nothing -> False
      Just (Constant (e:es)) -> e == letter

-- Predicates
male x          = List [ Constant "male",        x    ]
female x        = List [ Constant "female",      x    ]
father x y      = List [ Constant "father",      x, y ]
mother x y      = List [ Constant "mother",      x, y ]
sibling x y     = List [ Constant "sibling",     x, y ]
grandFather x y = List [ Constant "grandFather", x, y ]
ancestor x y    = List [ Constant "ancestor",    x, y ]
likes x y       = List [ Constant "likes",       x, y ]

bob     = Constant "bob"
lisa    = Constant "lisa"
john    = Constant "john"
paul    = Constant "paul"
lea     = Constant "lea"
lola    = Constant "lola"
fred    = Constant "fred"
albert  = Constant "albert"
mehdi   = Constant "mehdi"

cat     = Constant "cat"
sweetie = Constant "sweetie"

x       = Var "x"
y       = Var "y"
z       = Var "z"

facts = [ father bob john
        , father bob albert
        , father albert lola
        , ancestor x y :- [ father x y ]
        , ancestor x y :- [ father x z, ancestor z y ] ]

query
  = father x y
    :-  [ father z x ]
    :<< [ startWith x 'j' ]

test2 = case solve [] facts (father bob x) of
            Nothing -> print "Nothing"
            Just bs -> print $ bindingsToString bs

test = case solve [] facts query of
          Nothing -> print "Nothing"
          Just bindings -> print $ bindingsToString bindings
