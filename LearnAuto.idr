
data Foo : Type where
  MkFoo : Int -> Foo
  
foo2 : {auto x : Foo} -> Int
foo2 @{MkFoo v} = v


foo1 : Int
foo1 =
  let x = MkFoo 1
      --y = MkFoo 2 -- won't allow multiple solutions
  in
     foo2
     
foo3 : Int
foo3 =
  let x = MkFoo 1
      y = MkFoo 2
  in
     foo2 {x=x}

data Foo2 : Type where 
  WhoCaresFoo2 : Foo2
  WhoCaresAgainFoo2 : Foo2
  YouCanDoItFoo2 : Int -> Foo2
  CantYouFoo2 : Void -> Foo2

--multiple solutions are allowed in top level, but not in lets
foo4 : {auto x : Foo2} -> Foo2
foo4 @{WhoCaresFoo2} = WhoCaresFoo2
foo4 @{WhoCaresAgainFoo2} = WhoCaresAgainFoo2
foo4 @{YouCanDoItFoo2 x} = YouCanDoItFoo2 x
foo4 @{CantYouFoo2 x} = CantYouFoo2 x

foo5 : Foo2
foo5 = foo4

-- a value in the context overrides top level
foo6 : Foo2
foo6 = 
  let
     f = WhoCaresFoo2 -- this is chosen over the top level (foo5 returns WhoCaresAgainFoo2, but this returns WhoCaresFoo2
     --f2 = WhoCaresAgainFoo2 --can't have two options in context
  in foo4

data Foo3 : Int -> List Int -> Type where
  FHere : (x : Int) -> Foo3 x []
  FThere : {y : Int} -> Foo3 x xs -> Foo3 x (y :: xs)
  
-- foo7 : {w : Int} -> Foo3 w 
-- foo7 w =
--   let x = FThere {y=1} (FThere {y=2} (FHere 3)) 
--   in
--     x
  
data Ref : (l : label) -> Type -> Type where
  [search l]
       MkRef : Maybe a -> Ref x a


magicallyGetIt : {auto a : Ref "funky" Int} -> Int
magicallyGetIt @{MkRef (Just v)}  = v
magicallyGetIt @{MkRef (Nothing)}  = 0

foo8 : Int
foo8 = 
  let x = the (Ref "funky" Int) $ MkRef (Just 5) 
      y = the (Ref "funky2" Int) $ MkRef (Just 6) 
  in
    magicallyGetIt

magicTrick : Ref "funky" Int
magicTrick = MkRef (Just 7)

foo9 : Int
foo9 = 
   magicallyGetIt
   
data Ref2 : (l : label) -> Type -> Type where
  [search l] --still don't know what this does...
       MkRef2 : Int -> Ref2 x a


magicallyGetIt2 : {auto a : Ref2 "funky" Int} -> Int
magicallyGetIt2 @{MkRef2 v}  = v

foo10 : Int
foo10 = 
  let x = the (Ref2 "funky" Int) $ MkRef2 52 
      y = the (Ref2 "funky2" Int) $ MkRef2 62 
  in
    magicallyGetIt2

%hint
magicTrick2 : Ref2 "funky" Int
magicTrick2 = MkRef2 72

foo11 : Int
foo11 = 
   magicallyGetIt2

data Ref3 : (l : label) -> Type -> Type where
  MkRef3 : Bool -> Ref3 x Bool

magicallyGetIt3 : {auto a : Ref3 "funky" Bool} -> Bool
magicallyGetIt3 @{MkRef3 v} = v

--notice we don't need anything in context, even for a Bool, since it will populate it automatically with
--True (by accident I guess, or maybe it chooses the first value in the data)
foo12 : Bool
foo12 = magicallyGetIt3

--here we put an x in context, so the type checker will choose x over it's default.
foo13 : Bool
foo13 = let x = MkRef3 False in magicallyGetIt3

magicallyGetIt3' : {a : Ref3 "funky" Bool} -> Bool
magicallyGetIt3' {a=MkRef3 v} = v

--co : neither of these work, because a isn't auto
-- foo14 : Bool
-- foo14 = magicallyGetIt3'

-- foo15 : Bool
-- foo15 = let x = the (Ref3 "funky" Bool) (MkRef3 False)
--          in magicallyGetIt3'

magicallyGetIt2' : {auto 1 a : Ref2 "funky" Int} -> Int
magicallyGetIt2' @{MkRef2 v}  = v

-- :exec foo16 {a=MkRef2 5}  
foo16 : {1 a : Ref2 "funky" Int} -> IO Int
foo16 {a=a} = 
  let (MkRef2 x) = a
  in 
    do
      v <- pure magicallyGetIt2' --will not pull from the context (because it's linear I guess?)
      putStrLn $ show v
      pure v

-- :exec foo17 {a=MkRef2 5}  
foo17 : {a : Ref2 "funky" Int} -> IO Int
foo17 {a=a} = 
  let (MkRef2 x) = a
  in 
    do
      v <- pure magicallyGetIt2' -- but will pull here
      putStrLn $ show v
      pure v
     
data LabeledReq : String -> Type where
  [noHints]
  MkLabeledReq : LabeledReq v
 
forceTheDo : (f : (1 a : LabeledReq "f1") -> IO ()) -> IO ()
forceTheDo f = f (MkLabeledReq)

mustDoMe1 : (1 a : LabeledReq "f1") -> IO ()
mustDoMe1 (MkLabeledReq) = putStrLn "Ah, thank you! 1"

testDoingIt : IO ()
testDoingIt = forceTheDo $ (\a => 
    mustDoMe1 a)
    
-- forceTheDo : ({1 a : LabeledReq "f1"} -> {1 b : LabeledReq "f2"} -> IO ()) -> IO ()
-- forceTheDo f = f {a=MkLabeledReq} {b=MkLabeledReq}

-- mustDoMe1 : {auto 1 a : LabeledReq "f1"} -> IO ()
-- mustDoMe1 {a=MkLabeledReq} = putStrLn "Ah, thank you! 1"

-- mustDoMe2 : {auto 1 a : LabeledReq "f2"} -> IO ()
-- mustDoMe2 {a=MkLabeledReq} = putStrLn "Ah, thank you! 2"

-- testDoingIt : IO ()
-- testDoingIt = forceTheDo $
--   do
--     mustDoMe1
--     mustDoMe2
    
              
basic : {zz : Int} -> {auto 0 prf : zz = 5} -> Int
basic = 5

testBasic : Int
testBasic = basic

basic2 : {zz : Nat} -> {auto 0 prf : zz = 5} -> Nat
basic2 = 5

testBasic2 : Nat
testBasic2 = basic2

data Foo5 : {zz : Int} -> {auto 0 prf : zz = 5} -> Type where
  MkFoo5 : Foo5

testFoo5 : Int
testFoo5 = let 
               -- x : Foo5 {zz=zzt} -- co: doesn't work, can't unify
               x : Foo5 {zz=5}
               x = the Foo5 MkFoo5
           in 3

-- co : doesn't work, can't do auto here, fails at MkFoo6. So this means that auto is
--      realized in the data item "MkFoo6"
-- data Foo6 : {zz : Int} -> {yy : Int} -> {auto 0 prf : zz = yy} -> Type where
--   MkFoo6 : Foo6 

-- it seems that auto, even in the data item, won't search within the function
data Foo7 : {zz : Int} -> {yy : Int} -> Type where
  [search zz yy] --doesn't make a difference, here or not
  -- need to specify zz and yy as args to MkFoo7 or auto doesn't seem to work, no matter what we do
  MkFoo7 : {zz : Int} -> {yy : Int} -> {auto 0 prf : zz = yy} -> Foo7 {zz=zz} {yy=yy}
  MkFoo7' : {t : Int} -> {auto 0 prf : zz = yy} -> Foo7 {zz=t} {yy=t}

testFoo7 : Int
testFoo7 = let
              -- x : Foo7 {zz=w} {yy=w}
              x = the (Foo7 {zz=5} {yy=5}) MkFoo7
              --x = the (Foo7 {zz=5} {yy=5}) MkFoo7' -- co: doesn't work
              --x = the Foo7 $ MkFoo7' {t=5} -- co: doesn't work
              --x = the (Foo7 {zz=5} {yy=5}) $ MkFoo7' {t=5} -- co: doesn't work
            in 5
           

data Foo8 : (zz : Int) -> (yy : Int) -> Type where
  -- need to specify zz and yy as args to MkFoo7 or auto doesn't seem to work, no matter what we do
  MkFoo8Same : {zz : Int} -> {yy : Int} -> (prf : zz = yy) -> Foo8 zz yy
  MkFoo8Diff : Foo8 zz yy

--findAFoo8 : (zz : Int) -> (yy : Int) -> {auto foo8 : Foo8 {zz=zz} {yy=yy}} -> Foo8 {zz=zz} {yy=yy}
findAFoo8 : (zz : Int) -> (yy : Int) -> {auto foo8 : Foo8 zz yy} -> Foo8 zz yy
findAFoo8 z y =  foo8

aFoo8 : Foo8 1 1
aFoo8 = findAFoo8 1 1

--answer: diff
testFoo8 : String
testFoo8 = case aFoo8 of
             (MkFoo8Same prf) => "same"
             MkFoo8Diff => "diff"

-- this only matches "same" if we comment out MkFoo9Diff                          
data Foo9 : (zz : Int) -> (yy : Int) -> Type where
  -- need to specify zz and yy as args to MkFoo7 or auto doesn't seem to work, no matter what we do
  MkFoo9Same : {zz : Int} -> Foo9 zz zz
  -- MkFoo9Diff : Foo9 zz yy

--findAFoo9 : (zz : Int) -> (yy : Int) -> {auto foo9 : Foo9 {zz=zz} {yy=yy}} -> Foo9 {zz=zz} {yy=yy}
findAFoo9 : (zz : Int) -> (yy : Int) -> {auto foo9 : Foo9 zz yy} -> Foo9 zz yy
findAFoo9 z y =  foo9

aFoo9 : Foo9 1 1
aFoo9 = findAFoo9 1 1

--answer: diff
testFoo9 : String
testFoo9 = case aFoo9 of
             MkFoo9Same => "same"
             -- MkFoo9Diff => "diff"
             
--so auto is unpredicatable what it will match and what it will not
--since we can't use LEM, we can't tell whether the Type == () or not
-- this only matches "same" if we comment out MkFoo9Diff                          



data Foo10 : (t : Type) -> Type where
  -- need to specify zz and yy as args to MkFoo7 or auto doesn't seem to work, no matter what we do
  MkFoo10Char : Foo10 Char
  MkFoo10Other : Foo10 t

findAFoo10 : (t : Type) -> {auto foo10 : Foo10 t} -> Foo10 t
findAFoo10 t =  foo10

aFoo10 : Foo10 Char
aFoo10 = findAFoo10 Char

--answer: diff
testFoo10 : String
testFoo10 = 
     case findAFoo10 Char of
             MkFoo10Char => "same"
             MkFoo10Other => "oth"

basic3 : (yy : Nat) -> (zz : Nat) -> {auto 0 prf : zz = yy} -> Nat
basic3 a1 a2 = a1

testBasic3 : Nat
testBasic3 = basic3 3 3

data Ordering' = LT | EQ | GT

-- interface Eq' a where
--   (===) : a -> a -> Bool

-- interface Eq' a => Ord' a where
--    compare : a -> a -> Ordering'

-- Eq' Int where
--    a === b = a == b
   
-- Ord' Int where
--    compare a b =
--      if a < b then LT else if a > b then GT else EQ

data Eq' : (a : Type) -> Type where [noHints]
  MkEq' : (eq' : a -> a -> Bool) -> Eq' a
  
data Ord' : a -> Type where [noHints]
  MkOrd' : {auto 0 _ : Eq' a} -> (compare' : a -> a -> Ordering') -> Ord' a
  
compare' : {auto o : Ord' a} -> a -> a -> Ordering'
compare' {o = MkOrd' com} x y = com x y
  
%hint 
EqInt' : Eq' Int
EqInt' = MkEq' eq_meth' where
  eq_meth' : Int -> Int -> Bool
  eq_meth' a b = a == b
  

%hint
OrdInt' : Ord' Int
OrdInt' = MkOrd' compare_meth where
  compare_meth : Int -> Int -> Ordering'
  compare_meth a b = 
      if a < b then LT else if a > b then GT else EQ

max' : Ord' a => a -> a -> a
-- max x y = x
max' x y = case compare' x y of
               LT => y
               EQ => x
               GT => x
               
test : Int
test = let 
           x : Int
           x = 5
           y : Int
           y = 6
        in max' x y

