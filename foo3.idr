--reimplementation of a dependent pair
data Res' : ( a : Type ) -> ( a -> Type ) -> Type where
   (@@&) : ( val : a ) -> (1 resource : r val ) -> Res' a r

--ignores entire Res   
foo38 : (x : Res' Bool (\v => if v then Int else Char)) -> Int
foo38 _ = 5

--uses "val" of Res but ignores "resource"
foo39 : (x : Res' Bool (\v => if v then Int else Char)) -> Int
foo39 ((@@&) a r) = if a then 5 else 17

-- --uses "val" of Res in a linear context but ignores "resource" (doesn't type check)
-- foo40 : (1 x : Res' Bool (\v => if v then Int else Char)) -> Int
-- foo40 ((@@&) a r) = if a then 5 else 17

--same as Res' but resource is not linear
data Res'' : ( a : Type ) -> ( a -> Type ) -> Type where
   (@@&&) : ( val : a ) -> (resource : r val ) -> Res'' a r
   
--ignores entire Res   
foo41 : (x : Res'' Bool (\v => if v then Int else Char)) -> Int
foo41 _ = 5

--uses "val" of Res but ignores "resource"
foo42 : (x : Res'' Bool (\v => if v then Int else Char)) -> Int
foo42 ((@@&&) a r) = if a then 5 else 17

--uses "val" of Res in a linear context but ignores "resource" (doesn't type check)
foo43 : (1 x : Res'' Bool (\v => if v then Int else Char)) -> Int
foo43 ((@@&&) a r) = if a then 5 else 17

