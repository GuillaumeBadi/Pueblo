
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

assoc :: Expression -> Expression -> Binding Expression
assoc k v = mappend mempty $ Binding (Map.insert k v Map.empty)

getValueAt v (Binding m) = Map.lookup v m

substitute :: Binding Expression -> Expression -> Expression
substitute m c | constant c = c
substitute m (ListE l) = ListE $ map (substitute m) l
substitute m v@(PlaceholderE _) = fromMaybe v (getValueAt v m)

constant :: Expression -> Bool
constant (DoubleE _) = True
constant (IntE _) = True
constant (StringE _) = True
constant _ = False

constraint :: Expression -> Bool
constraint (_ :- _) = True
constraint _ = False

placeholder (PlaceholderE _) = True
placeholder _ = False

fact (ListE _) = True
fact _ = False

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

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _ -> False

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

substituteAll :: [Expression] -> Binding Expression -> [Expression]
substituteAll l b = map (substitute b) l

mergeBindings :: Binding Expression -> Bindings Expression -> Bindings Expression
mergeBindings b (Bindings bs) = Bindings $ map (<> b) bs

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

facts = makeFacts [ "romeo loves juliet"
                  , "X loves Y if Y loves X" ]

query = readFact "juliet loves Who"
