
module Helpers where

import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map
import           Data.Map.Lazy    (foldlWithKey)
import           Data.Monoid

import Types
import Parse

assoc :: Expression -> Expression -> Binding Expression
assoc k v = mappend mempty $ Binding (Map.insert k v Map.empty)

keepBinding :: Expression -> [Expression]
keepBinding (ListE query) = foldl ft [] query
  where ft a c@(PlaceholderE _) = c:a
        ft a _ = a
keepBinding _ = []

filterBinding :: Expression -> Bindings Expression -> Bindings Expression
filterBinding query (Bindings bs) = Bindings $ foldl flt [] bs
  where flt a (Binding b) = (Binding $ Map.filterWithKey fltMap b) : a
        fltMap k v = any (== k) keep
        keep = keepBinding query

makeFacts :: [String] -> [Expression]

makeFacts = map readFact

mergeBindings :: Binding Expression -> Bindings Expression -> Bindings Expression
mergeBindings b (Bindings bs) = Bindings $ map (<> b) bs
