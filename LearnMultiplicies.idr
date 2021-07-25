module LearnMultiplicies

data Foo : Int -> Type where
  FFoo : (1 x : Int) -> Foo x
  
data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  Cons : a -> Vect n a -> Vect (S n) a
  
--addEm : Vect m Int -> Vect m Int -> Vect m Int

  
append : (1 xs : List a ) -> (1 ys : List a ) -> List a
append [] ys = ys
--append (x :: xs) ys = x :: ys -- all the elements of xs have to also be used only once
--append (x :: xs) ys = x :: x :: append xs ys --cannot use x twice, as it is an element of xs
append (x :: xs') ys = x :: append xs' ys

data Bla : Type where
   MkBla : (1 x : Int) -> Bla
   
data Bla2 : Type where
   MkBla2 : (x : Int) -> Bla2
   
foo : (x : Int) -> Bla
foo x = MkBla x

foo' : (1 x : Int) -> Bla
foo' x = MkBla x

foo2 : (x : Int) -> Bla2
foo2 x = MkBla2 x

-- won't work, demands linear Bla, but I don't see why all data shouldn't 
-- have a "1 " before types of its args. Otherwise you can't use it for 
-- a linear function, but having a "1" doesn't affect non-linear functions.
-- So what's the deal?
--
-- Im mean, see fickle', we can extract out the x and use it 
-- foo2' : (1 x : Int) -> Bla2
-- foo2' x = MkBla2 x

fee : (b : Bla) -> (Int,Int)
fee (MkBla x) = (x,x)

fee' : (1 b : Bla) -> (Int,Int)
fee' (MkBla x) = (x,5)

fickle : (1 x : Int) -> (Bla,Int)
fickle x = let b = MkBla x
               (MkBla x') = b
           in (MkBla x',5) -- you can't create more than one Bla here

fickle' : (x : Int) -> (Bla,Int)
fickle' x = let b = MkBla x
            in (b,x)

data MyWorld : Type where

data MyIORes : Type -> Type where
     MkMyIORes : (result : a) -> (1 x : MyWorld) -> MyIORes a
     
MyPrimIO : Type -> Type
MyPrimIO a = (1 x : MyWorld) -> MyIORes a

data MyIO : Type -> Type where
  MkMyIO : (1 p : MyPrimIO a) -> MyIO a
  
io_bind : (1 act : MyIO a) -> (1 k : a -> MyIO b) -> MyIO b
io_bind (MkMyIO primAct) k = MkMyIO (\prevWorld => 
     let (MkMyIORes aRes afterAWorld) = primAct prevWorld
         (MkMyIO bIO) = k aRes
     in
        bIO afterAWorld)
        
-- MkMyIO (\w => 
--    let ar = 


-- cantSquare : (1 x : Int) -> Int
-- cantSquare x = x * x


myGreaterThan : (1 x : Nat) -> Nat -> Bool
myGreaterThan 0 k = False
myGreaterThan (S 0) 0 = True
myGreaterThan (S (S k)) 0 = myGreaterThan (S k) 0
myGreaterThan (S x') (S k') = myGreaterThan x' k'

--trying to get around linearity (for fun, learning)
useButNotUse : (1 x : Nat) -> ()
useButNotUse x = case myGreaterThan x 5 of 
                      True => ()
                      False => ()

useButNotUse2 : (1 x : Nat) -> ()
useButNotUse2 0 =  ()
useButNotUse2 (S k) = useButNotUse2 k
                      
useButNotUse3 : (1 a : Type) -> ()
useButNotUse3 a = case a of 
                    (Int) => ()
                    _ => ()


-- Doesn't work???                                       
-- useButNotUse3' : (1 a : Type) -> ()
-- useButNotUse3' (Int) = ()
-- useButNotUse3' _ = ()

                    
useButNotUse4 : (1 x : Nat) -> Int
useButNotUse4 a = case a of 
                     0 => 5                                                            
                     (S k) => useButNotUse4 k                                                            
                     
useButNotUse5 : (1 x : Nat) -> Int
useButNotUse5 a = case a of 
                     0 => 5                                                            
                     _ => 5                                                            
useButNotUse6 : (1 x : Int) -> Int
useButNotUse6 a = case a of 
                     0 => 5                                                            
                     _ => 5                                                            
useButNotUse7 : (1 x : Double) -> Int
useButNotUse7 a = case a of 
                     0 => 5                                                            
                     _ => 5                                                            
-- useButNotUse5 : (1 x : Nat) -> Int
-- useButNotUse5 a = case a of 
--                      _ => 5                                                            

data IsJust : Maybe a -> Type where
  ItIsJust : IsJust ( Just val )
  
  
fromMaybe : ( x : Maybe a ) -> { auto 0 p : IsJust x } -> a
fromMaybe ( Just x ) { p = ItIsJust } = x

-- foo28 : (0 x : Int) -> Int
-- foo28 _ = 5

-- note: this doesn't work because + doesn't take linear args!
-- foo27 : (1 x : Int) -> Int
-- foo27 x = foo28 x + x


foo28 : (0 x : Bool) -> Bool
foo28 _ = True

linearOr : (1 x : Bool) -> (1 y : Bool) -> Bool
linearOr True True = True
linearOr True False = True
linearOr False y = y

foo27 : (1 x : Bool) -> Bool
foo27 x = linearOr (foo28 x) x

foo30 : (1 x : Int) -> Int
foo30 x = x

foo32 : (1 x : Int) -> Int
foo32 x = case x of 
            0 => 0
            _ => 1
            
-- foo32' : (1 x : Int) -> Int
-- foo32' 0 = 0
-- foo32' _ = 1

foo29 : Int
foo29 =
  let z = 5
      z2 = foo30 z
  in
     z + z2
     
foo31 : (1 x : Int) -> Int
foo31 x =
  let z = 5
      z2 = foo32 x
  in
     z2
     
foo35 : (1 x : Int) -> Int
foo35 x =
  let z = 5
      z2 = foo31 x
  in
     z2
     
foo34 : (1 x : Bool) -> Bool
foo34 x = case x of 
            False => True
            True => False

foo37 : (1 x : Bool) -> Bool
foo37 x =
  let z = True
      z2 = foo34 x
  in
     z2 


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

data Lin : Type -> Type where
     MkLin : (1 _ : a) -> Lin a

data Unr : Type -> Type where
     MkUnr : a -> Unr a

-- getLin : (1 _ : Lin a) -> a
-- getLin (MkLin x) = ?howmanyLin

-- getUnr : (1 _ : Unr a) -> a
-- getUnr (MkUnr x) = ?howmanyUnr

foo44 : (1 x : Bool) -> (y : Bool) -> Bool
foo44 x y = case x of
              True => y
              False => not y
              
-- foo45 : (1 x : Bool) -> Bool
-- foo45 x = let
--             foo44ish = foo44 x -- foo44ish becomes linear
--             res = foo44ish True
--           in
--             ?xxx
--             --False
--             --res
            
foo46 : (1 x : Bool) -> ()
foo46 True = ()
foo46 False = ()

foo47 : (x : Bool) -> Int
foo47 x =
  let v = foo46 x
  in
    5
                  
-- foo48 : (x : Bool) -> (1 y : Bool) -> Int

-- foo49 : (1 y : Bool) -> Int
-- foo49 y =
--   let afoo48 = foo48 True y
--   in
--     ?xxx

data Limit : Int -> Type where
     MkLimit : Int -> Limit x

foo50 : (0 x : Int) -> Int -> Limit x
foo50 x y = MkLimit y

foo51 : (x : Int) -> Limit x
foo51 x = foo50 x x


useX : (1 x : Int) -> ()
useX x = case x of
               1 => ()
               _ => ()
               
foo52 : (1 x : Int) -> ()
foo52 x = useX x
  -- do 
  --   pure $ useButNotUse5 x
  --   putStrLn "Bonzai!"

  
v : (0 x : Nat) -> Nat
v _ = Z

v2 : (0 x : Nat) -> Nat
v2 x = v x

v2' : (0 x : Nat) -> Nat
v2' x = v (x+1)

v2'' : (0 x : Nat) -> Nat
v2'' x = let 0 xp = (x+1) --note "let 0" here
         in v xp
         
w : (1 x : Nat) -> Nat
w x = x

w2'' : (1 x : Nat) -> Nat
w2'' x = let 1 xp = S x  
         in w xp
         
w2''' : (1 x : Nat) -> Nat
w2''' x = let xp = S x  --let 1 is not needed, though
         in w xp
         
-- w2 : (1 x : Nat) -> Nat
-- w2 x = w x

-- w2' : (1 x : Nat) -> Nat
-- w2' x = w (S x)

