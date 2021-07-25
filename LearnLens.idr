import Data.SortedMap


A1 : Functor f => (a -> f a) -> (a,b) -> f (a,b)
A1 f (a, b) = map (\a' => (a',b)) (f a)

data Foo = MkFoo Int Int

-- fooa :: Lens' (Foo a) Int
fooa : Functor f => (Int -> f Int) -> Foo -> f Foo
fooa f (MkFoo a b) = map (\a' => MkFoo a' b) (f a)

A1fooa : Functor f => (Int -> f Int) -> (Foo,b) -> f (Foo,b)
A1fooa = A1 . fooa

testA1fooa : Maybe (Foo, Char)
testA1fooa = A1fooa (Just . (+1)) (MkFoo 1 2,'x')

data Foo2 = MkFoo2 (SortedMap Char Foo)

foo2a : Functor f => (Foo -> f Foo) -> (Foo2,Char,Foo) -> f (Foo2,Char,Foo)
foo2a f (MkFoo2 mp,key,defFoo) = 
   let foo : Foo
       foo = maybe defFoo (\a => a) (lookup key mp)
   in
      (map 
        (\foo => (MkFoo2 $ insert key foo mp, key, defFoo)) 
        $ f foo ) 

data Foo3 = MkFoo3 (SortedMap Int Foo2)

foo3a : Functor f => (Foo2 -> f Foo2) -> (Foo3,Int,Foo2) -> f (Foo3,Int,Foo2)
foo3a f (MkFoo3 mp,key,defFoo2) = 
   let foo2 : Foo2
       foo2 = maybe defFoo2 (\a => a) (lookup key mp)
   in
      (map 
        (\foo2 => (MkFoo3 $ insert key foo2 mp, key, defFoo2)) 
        $ f foo2 ) 

foo321 : Functor f => (Int, Foo2) -> (Char,Foo) -> Int -> f Int -> Foo3 -> f Foo3
