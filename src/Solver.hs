
module Solver where

import           Types
import           TypeCheck
import           Unify
import           Substitue

import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

satisyFilters :: [Filter] -> Binding -> Bool
satisyFilters [] b = True
satisyFilters (f:fs) b = if f b then satisyFilters fs b else False

{- Takes a list of fact and a query,
    we map the facts to match the query. It the current fact is not a constraint,
    We just unify it, otherwise, we can call solveWithConstraints to solve it -}

solve :: [Expression]
      -> [Expression]
      -> Expression
      -> Maybe [Binding]

solve acc fs (q :<< filters)
  = case solve acc fs q of
      Nothing -> Nothing
      Just bindings -> case filter (satisyFilters filters) bindings of
        [] -> Nothing
        bs -> Just bs

solve acc fs (q :- constraints)
  = case solveConstraints acc fs (q : constraints) q of
      Nothing -> Nothing
      Just bindings -> Just bindings

solve acc fs q | q `elem` acc = Just []

solve acc fs query = case mapMaybe process fs of
                      [] -> Nothing
                      bindings -> Just $ concat bindings
  -- Takes a fact, and choose between unify and solveWithConstraints
  where process = (\f -> if isConstraint f
                           then solveWithConstraints (query : acc) fs query f
                           else case unify f query of
                             Nothing -> Nothing
                             Just binding -> Just [binding])


mergeBindings :: Binding -> [Binding] -> [Binding]
mergeBindings b bs = map (Map.union b) $ bs

{- Given a list of facts, a list of constraints, and an expression e
   If the list of constraints is empty, return [ ]
   else If e is an excluded constraint, return [ ]
   else we get the bindings from solving x. If Nothing -> Nothing
      otherwise we test each branch with the bindings and returns
      the filtered result -}
solveConstraints :: [Expression]
                 -> [Fact]
                 -> [Expression]
                 -> Expression
                 -> Maybe [Binding]

solveConstraints acc fs [] e = Just [Map.empty]

solveConstraints acc fs (x:xs) e
  = case solve acc fs x of
      Nothing -> Nothing
      Just bindings -> do
         let branches = zip bindings (map (substituteAll xs) bindings)
         let tests = mapMaybe testBranch branches
         if null tests then Nothing else Just $ concat tests

  where testBranch = \(bs, branch) ->
          case solveConstraints acc fs branch e of
             Nothing -> Nothing
             Just res -> Just $ mergeBindings bs res

        predicate = (\(List (x:_) :- _) -> x)

{- pre-solver for Constraints:
    given a query and some constraints, we must
    substitute with the query bindings, all the constraint expressions to
    make it easier to get the result back to the caller -}
solveWithConstraints :: [Expression] --
                     -> [Expression] --
                     -> Expression   --
                     -> Constraint   --
                     -> Maybe [Binding]

solveWithConstraints acc fs q f@(c :- l)
  = case unify c q of
      Nothing      -> Nothing
      Just binding -> do
        let exprs = map (substitute binding) l
        solveConstraints (substitute binding c : acc) fs exprs f

boolSolver :: [Expression] -> Expression -> Bool
boolSolver f q = case solve [] f q of
                      Nothing -> False
                      Just _  -> True

extractVaribles :: Expression -> [Expression]
extractVaribles v@(Var e)      = [v]
extractVaribles (Constant e)   = []
extractVaribles (List l)       = concatMap extractVaribles l

getSolver ::[Expression] -> Expression -> Maybe [Binding]
getSolver f q = case solve [] f q of
  Nothing       -> Nothing
  Just bindings -> Just bindings
