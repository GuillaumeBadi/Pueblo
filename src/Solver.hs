
module Solver where

import           Data.Maybe
import           Data.Monoid

import Types
import TypeCheck
import Substitute
import Unify
import Helpers

solveConstraints :: [Expression]
                 -> [Expression]
                 -> [Expression]
                 -> Maybe (Bindings Expression)

solveConstraints _ _ [] = (Just . Bindings) [mempty]

solveConstraints acc fs (x:xs) = do
  (Bindings bindings) <- solve acc fs x
  let branches = zip bindings (map (substituteAll xs) bindings)
  let tests = mapMaybe (testBranch acc fs) branches
  if null tests
    then Nothing
    else return (foldl (<>) mempty tests)

testBranch :: [Expression]
            -> [Expression]
            -> (Binding Expression, [Expression])
            -> Maybe (Bindings Expression)

testBranch acc fs (b, branch) =
  fmap (mergeBindings b) (solveConstraints acc fs branch)

solveWithConstraints :: [Expression]
                     -> [Expression]
                     -> Expression
                     -> Expression
                     -> Maybe (Bindings Expression)

solveWithConstraints acc fs q f@(c :- l) = do
  binding <- unify c q
  let exprs = map (substitute binding) l
  b <- solveConstraints (substitute binding c : acc) fs exprs
  return b

solve :: [Expression]
      -> [Expression]
      -> Expression
      -> Maybe (Bindings Expression)

solve acc fs (q :- constraints) =
  solveConstraints acc fs (q : constraints)

solve acc fs q | q `elem` acc = Just . Bindings $ []

solve acc fs query =
  case mapMaybe process fs of
      [] -> Nothing
      bindings -> Just . (filterBinding query) $ foldl (<>) mempty bindings
  where process f
          | constraint f = solveWithConstraints (query : acc) (filter (/= query) fs) query f
          | otherwise = unify f query >>= \b -> return . Bindings . (b:) $ []
