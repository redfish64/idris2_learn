module LearnWeirdStuff3 

import Data.SortedMap
import Data.Vect
import LearnWeirdStuff3h

foo : Int
foo = 42

foo2 : Int
foo2 = foo

interface KS x k v where
  getMap : x -> SortedMap k v

get : KS x k v => x -> k -> Maybe v
get x k = lookup k (getMap x)

data Event = E Int
data ExcRate = ER Double

Book : Type
Book = (SortedMap String Event, SortedMap Nat ExcRate)

KS Book String Event where
  getMap = fst
   
KS Book Nat ExcRate where
  getMap = snd
   

-- won't work because myadd isn't public export
-- fv : Vect (myadd 2 3) Int 
-- fv = [1,2,3,4,5]

interface Foo (a : Type) where
  foov : Type

Foo Int  where
  foov = Int
  
Foo a where
  foov = a
  
  


