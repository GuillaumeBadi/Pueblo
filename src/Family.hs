
module Family where

import Pueblo

-- Predicates
male x = List [Constant "male", x]
female x = List [Constant "female", x]
parent x y = List [Constant "parent", x, y]
father x y = List [Constant "father", x, y]
mother x y = List [Constant "mother", x, y]
grandFather x y = List [Constant "grandFather", x, y]

bob = Constant "bob"
lisa = Constant "lisa"
john = Constant "john"
paul = Constant "paul"
lea = Constant "lea"
lola = Constant "lola"

x = Var "x"
y = Var "y"
z = Var "z"

who = Var "Who"

facts =
 [ father bob lola
 , mother lea lola
 , father john bob
 , grandFather x y :- [ father x z, father z y ] ]

test = boolSolver facts (grandFather john bob) -- False
test2 = boolSolver facts (grandFather john lola) -- True
