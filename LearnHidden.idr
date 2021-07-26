import Data.List


interface Foo x where
  length : x -> Nat
  
Foo (List Bool) where
  length x = List.length x + 10
 
--without this, test1 won't compile 
%hide List.length

test1 : Nat
test1 = length [True,False,True]
