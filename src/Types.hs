
module Types where

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map
import           Data.Map.Lazy    (foldlWithKey)
import           Data.List

data Expression
  = PlaceholderE String
  | StringE String
  | IntE Integer
  | DoubleE Double
  | BoolE Bool
  | ListE [Expression]
  | Expression :- [Expression]
  deriving (Ord, Eq)

data Binding a = Binding (Map a a)

data Bindings a = Bindings [Binding a]

instance Show a => Show (Bindings a) where
  show (Bindings []) = "Yes"
  show (Bindings bs) = intercalate "" (map show bs)

instance Monoid (Bindings a) where
  mempty = Bindings []
  mappend (Bindings from) (Bindings to) = Bindings $ from ++ to

bindingStringReducer a k v =
  a ++
  show k ++ ": " ++ show v ++
  "\n"

instance Ord a => Monoid (Binding a) where
  mempty = Binding Map.empty
  mappend (Binding from) (Binding to) = Binding $ Map.union from to

instance Show a => Show (Binding a) where
  show (Binding m) | (length $ Map.toList m) == 0 = "Yes"
  show (Binding m) = "\n" ++ foldlWithKey bindingStringReducer "" m

instance Monoid Expression where
  mempty = ListE []
  mappend a b = ListE [a, b]

instance Show Expression where
  show (ListE l) = intercalate " " (map show l)
  show (StringE s) = s
  show (IntE i) = show i
  show (BoolE b) = show b
  show (DoubleE d) = show d
  show (e :- e') = show e ++ " if " ++ (intercalate " and " (map show e'))
  show (PlaceholderE p) = p
