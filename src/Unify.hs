module Unify where

import Types
import TypeCheck
import Substitue

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

{- Takes a Variable and an Expression, and returns:
        True is the variable occurs in the Expression
        otherwise False-}
occurs :: Expression -> Expression -> Bool
occurs v          (Constant _) = False
occurs (Var v)    (Var v')     = v == v'
occurs var        (List l)     = any (occurs var) l

{- We want an algorithm that can take two expressions:
    e1 f(g(a)) + X
    e2 f(Y) + 3
  And returns the value of Y and X, here Y: g(a) and X: 3

  If the Unification fails, we return Nothing
  Otherwise, a map with variables values :
    [(X, Constant 3), (Y, List g(a))] -}
unify :: Expression -> Expression -> Maybe Binding

{- If both expressions are Constants:
    We cannot unify them if they are different:
      i.e. e1: 2, e2: 3 -> Nothing
    We can unify them with an empty Map is they are the same:
      i.e. e1: 2, e2: 2 -}
unify (Constant x) (Constant y) = if x == y then Just Map.empty else Nothing

{- If one expression is a Variable:
      if the other one is a List, and that list contains the variable,
        that would recur forever, so we fail
        i.e. e1: x, e2: f(x) -> x: f(f(f(f(f(f(f(...x)))))))
      else we can set the variable to the expression -}

-- The variable is passed first
unify var@(Var _) e2
  | isList e2 && occurs var e2 = Nothing
  | otherwise = Just (Map.insert var e2 Map.empty)

-- The Variable is passed second
unify e1 var@(Var _)
  | isList e1 && occurs var e1 = Nothing
  | otherwise = Just (Map.insert var e1 Map.empty)

-- Now if one of the expression is not a List, it should fail,
-- Technically, this will not appear -}
unify e1 e2
  | (not . isList $ e1) &&
    (not . isList $ e2)
    = Nothing

-- If only one expression is empty, it means they
-- do not share the same length -> Nothing
-- If they are both empty, return an empty Map
unify l1@(List e1) l2@(List e2)
  | null e1 || null e2
    = if null e1 /= null e2 then Nothing else Just Map.empty

{- If we have two non-empty lists
    we unify the first element of both lists recursively.
    Then we try the new substitution with the rest of both expression
    and backtrack if further unification fails -}
unify (List e1@(h1:t1)) (List e2@(h2:t2))
  -- We unify the first parts of both lists
  = case unify (h1 :: Expression) (h2 :: Expression) of
         Nothing    -> Nothing
        --  With the returned bindings:
         Just subs1 -> do
           -- We apply these bindings to the rest of both expression
           let te1 = substitute subs1 (List t1)
           let te2 = substitute subs1 (List t2)
           -- We try to unify them
           case unify te1 te2 of
                -- if it failed, we can backtrack
                Nothing    -> Nothing
                -- If it succeeded, the bindings were correct,
                -- So we can merge them
                Just subs2 -> Just (Map.union subs1 subs2)
