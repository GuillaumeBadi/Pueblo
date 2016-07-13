
module Family where

import Pueblo

-- Predicates
male x = List [Constant "male", x]
female x = List [Constant "female", x]
parent x y = List [Constant "parent", x, y]
father x y = List [Constant "father", x, y]
mother x y = List [Constant "mother", x, y]
uncle x y = List [Constant "uncle", x, y]
brother x y = List [Constant "brother", x, y]
grandFather x y = List [Constant "grandFather", x, y]
ancestor x y = List [Constant "ancestor", x, y]
grandgrandFather x y = List [Constant "grandgrandFather", x, y]
has x y = List [Constant "has", x, y]
pet x y = List [Constant "pet", x, y]

bob = Constant "bob"
lisa = Constant "lisa"
john = Constant "john"
paul = Constant "paul"
lea = Constant "lea"
lola = Constant "lola"
fred = Constant "fred"
albert = Constant "albert"
mehdi = Constant "mehdi"

cat = Constant "cat"
sweetie = Constant "sweetie"

x = Var "x"
y = Var "y"
z = Var "z"
a = Var "a"
v = Var "v"
w = Var "w"

who1 = Var "who1"
who2 = Var "who2"

facts = [ father bob lola
        , has mehdi (pet cat sweetie)
        , brother bob paul
        , mother lea lola
        , father john bob
        , father fred john
        , father albert fred
        , brother albert fred
        , brother x y :- [ brother y x ] -- x is y's brother if y is x's brother
        , grandFather x y :- [ father x z, father z y ]
        , grandgrandFather v w :- [ grandFather v z, father z w ]
        , parent x y :- [ father x y ]
        , parent x y :- [ mother x y ] ]
        -- , uncle x y :- [ parent z y, brother x z ]
        -- , aunt x y :- [ parent z y, sister x z ]
        -- , cousin x y :- [ parent z x, parent w y, brotherOrSister z w ]
        -- , parent x y :- [ father x y ]
        -- , parent x y :- [ mother x y ]
        -- , brotherOrSister x y :- [ brother x y ]
        -- , brotherOrSister x y :- [ sister x y ]

test = case solve facts (grandFather who1 lola) of -- (who1: john) 
            Nothing -> print "Nothing"
            Just bs -> print $ bindingsToString bs
