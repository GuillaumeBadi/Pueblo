
module Parse where

import           Data.Char
import           Data.List.Split

import Types
import TypeCheck

readElement :: String -> Expression
readElement x
  | isInteger x = IntE (read x :: Integer)
  | isDouble x = DoubleE (read x :: Double)
  | isUpper (x !! 0) = PlaceholderE x
  | x == "true" = BoolE True
  | x == "false" = BoolE False
  | otherwise = StringE x

readConstraints :: [String] -> Expression
readConstraints (x:xs:[]) =
  (readElements x) :- (map readElements cs)
  where cs = splitOn " and " xs

readConstraints _ = error "Error"

readElements :: String -> Expression
readElements xs = ListE $ map readElement (splitOn " " xs)

readFact :: String -> Expression
readFact s
  | (length cs) > 1 = readConstraints cs
  | otherwise = readElements (cs !! 0)
  where cs = splitOn " if " s
