import Control.Category


Arroro : Type -> Type -> Type
Arroro x y = x -> y

Category Arroro where
  id = Prelude.id
  (.) = Prelude.(.)

interface Category k => Isoc (0 k : Type -> Type -> Type) where
 isoc : (a -> b) -> (b -> a) -> k a b
 
Isoc Arroro where
  isoc ab ba = ab
 
data Isom a b = 
  MkIsom (a -> b) (b -> a)

Category Isom where
  id = MkIsom id id
  MkIsom bc cb . MkIsom ab ba = MkIsom (bc . ab) (ba . cb)
 
Isoc Isom where
  isoc = MkIsom

(<->) : Type -> Type -> Type
(<->) a b = {auto k : (ka : Type) -> (kb : Type) -> Type} -> Isoc k => k a b

infixl 7 <->

from : Isom a b -> (b <-> a)
from (MkIsom a b) = isoc b a

inc : Neg a => Num a => (a <-> a)
inc = isoc (+ 1) (+ (-1))

inc' : Neg a => Num a => ({k : (ka : Type) -> (kb : Type) -> Type} -> Isoc k => k a a)
inc' = isoc (+ 1) (+ (-1))

test1 : Int
test1 = inc {k=Arroro} 9

test2 : Int
test2 = (from {k=Arroro} $ inc {k=Isom}) 9

-- test1' : Int
-- test1' = inc' {k=_} 9

-- test2' : Int
-- test2' = (from {k=Arroro} $ inc' {k=Isom}) 9
  

