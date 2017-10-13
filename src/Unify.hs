module Unify where

import Types
import TypeCheck
import Substitute
import Helpers

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map

occurs :: Expression -> Expression -> Bool
occurs v x | constant x = False
occurs (PlaceholderE v) (PlaceholderE v') = v == v'
occurs var (ListE l) = (any $ occurs var) l

unify :: Expression -> Expression -> Maybe (Binding Expression)

unify var@(PlaceholderE _) e2
  | fact e2 && occurs var e2 = Nothing
  | otherwise = Just (assoc var e2)

unify e1 var@(PlaceholderE _)
  | fact e1 && occurs var e1 = Nothing
  | otherwise = Just (assoc var e1)

unify x y | constant x && constant y = if x == y then Just mempty else Nothing

unify l1@(ListE e1) l2@(ListE e2)
  | null e1 || null e2
    = if null e1 /= null e2 then Nothing else Just mempty

unify (ListE (h1:t1)) (ListE (h2:t2))
  = do
      s1 <- unify h1 h2
      let s = (substitute s1) . ListE
      s2 <- unify (s t1) (s t2)
      Just $ mappend s1 s2
