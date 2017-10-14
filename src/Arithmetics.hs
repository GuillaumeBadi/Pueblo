
module Instances where

import           Data.Char
import           Data.Monoid
import           Data.Char
import           Data.List.Split
import           Debug.Trace
import           Data.List
import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map
import           Data.Map.Lazy    (foldlWithKey)

import Parse
import Helpers
import Solver

facts = makeFacts [ "joao est le pere de rafael"
                  , "X est enfant de Y if Y est le pere de X"
                  , "rogerio est le pere de joao"
                  , "X est ancetre de Y if X est le pere de Y"
                  , "X est ancetre de Y if X est le pere de Z and Z est ancetre de Y "]

query = readFact "Qui est ancetre de rafael"

run = solve [] facts query
