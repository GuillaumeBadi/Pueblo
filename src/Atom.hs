
module Atom where

data Str = Str String deriving Show

data Placeholder = Placeholder Integer
  deriving (Eq, Show, Ord)

class LogicType a where
  constants :: [a]

instance LogicType Str where
  constants = [Str "Hello", Str "World"]

