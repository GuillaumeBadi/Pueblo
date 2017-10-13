
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

facts = makeFacts [ "romeo loves juliet"
                  , "X loves Y if Y loves X" ]

query = readFact "juliet loves Who"

run = solve [] facts query
