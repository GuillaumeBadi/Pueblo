
module TypeCheck where

import Types

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

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _ -> False
