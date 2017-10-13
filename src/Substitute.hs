
module Substitute where

import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map.Strict  as Map
import           Data.Map.Lazy    (foldlWithKey)

import Types
import TypeCheck

substitute :: Binding Expression -> Expression -> Expression
substitute m c | constant c = c
substitute m (ListE l) = ListE $ map (substitute m) l
substitute m v@(PlaceholderE _) = fromMaybe v (getValueAt v m)

substituteAll :: [Expression] -> Binding Expression -> [Expression]
substituteAll l b = map (substitute b) l

getValueAt v (Binding m) = Map.lookup v m
