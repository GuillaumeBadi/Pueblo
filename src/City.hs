
module Cities where

import Pueblo

import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

constantToInt :: Expression -> Int
constantToInt (Constant s) = read s :: Int

lessThan v val b = case Map.lookup v b of
  Nothing -> False
  Just c -> (constantToInt c) <= val

pop city n = List [ Constant "pop", city, n ]

paris = Constant "paris"
berlin = Constant "berlin"
lisboa = Constant "lisboa"

facts = [ pop paris (Constant "10")
        , pop berlin (Constant "11")
        , pop lisboa (Constant "9") ]

x = Var "x"
y = Var "y"

e = solve [] facts ((pop x y) :<< [ lessThan y 10 ])
