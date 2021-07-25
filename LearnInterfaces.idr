import Data.So
import Data.String


--can't figure out what this "determining paramenter" thing does
interface Foo x s | x where
  myfavnum : x -> s -> Int
  
  
Foo String s where
  myfavnum x s = strLength x

-- doesn't complain, hmm...  
Foo Char Int where
  myfavnum x s = 1 + s
  

test1 : Int
test1 =
  let fv : Int = myfavnum "hello" 3
      fv2 : Int = myfavnum 'c' 3 --no complaints here either
  in
    fv + fv2
 
--no problem 
Foo Char String where
  myfavnum x s = 1 + strLength s
 

test2 : Int
test2 =
  let fv : Int = myfavnum 'c' "hello"
  in
    fv

interface Foo2 x s where
  myfavnum2 : x -> s -> Int

Foo2 Char Int where
  myfavnum2 x s = 1 + s

test3 : Int
test3 =
  let fv2 : Int = myfavnum2 'c' (the Int 3) --here is the difference. If 3 is put in without "the Int" it
                                            --defaults to integer. It isn't cast to Int because the interface
                                            --hasn't been determined to be "Char" "Int" yet, so it looks for
                                            --char integer instead.
  in
    fv2

public export
data Prv : (x : Type) -> (x -> Bool) -> Type where
  MkPrv : (val : x) -> (prf : So (f val)) -> Prv x f
  
  
two : Prv Int (> 0)
two = MkPrv 2 Oh

one : Prv Int (> 0)
one = MkPrv 1 Oh

interface Addem x f1 f2 f3 | x,f1,f2 where
  (+) : Prv x f1 -> Prv x f2 -> Prv x f3

  
--note it only works with concrete types. Num x => Addem x ... will not match anything... I think it has
--something to do with the functions  
Addem Int (> 0) (> 0) (> 0) where
  (+) (MkPrv a _) (MkPrv b _) = MkPrv (a+b) (believe_me ())
  
Addem Int _ _ (const True) where
  (+) (MkPrv a _) (MkPrv b _) = MkPrv (a+b) (believe_me ())
  
interface Multem x f1 f2 f3 | x,f1,f2 where
  (*) : Prv x f1 -> Prv x f2 -> Prv x f3
  
Multem Int (> 0) (> 0) (> 0) where
  (*) (MkPrv a _) (MkPrv b _) = MkPrv (a*b) (believe_me ())
  
three : Prv Int (> 0)
three = one + two

interface Weaken x p1 p2 | x,p1 where
  weaken : Prv x p1 -> Prv x p2

{n : Nat} -> Weaken Int (> (cast n)) (> 0) where
  weaken (MkPrv val prf) = MkPrv val (believe_me ())
  
Weaken Int (> 0) (const True) where
  weaken (MkPrv val prf) = MkPrv val (believe_me ())

Weaken Int (> 0) (> (-1)) where
  weaken (MkPrv val prf) = MkPrv val (believe_me ())

Weaken x p1 p1 where
  weaken xv = xv

foo : Prv Int (const True)
foo = weaken three 

foo2 : Prv Int (> (-1)) 
foo2 = weaken $ (the (Prv Int (> 0)) $ one + two)


data Fir : (x : Type) -> (x -> Bool) -> Type where
  Fir1 : Fir Int (> 0) 
  


--doesn't work for some reason   
-- Weaken x p1 p1' => Weaken x p2 p2' => Addem x p1' p2' p3' => Addem x p1 p2 p3' where
--   (+) a b = weaken a + weaken b

-- foo2 : Prv Int (const True)
-- foo2 = one + two
   
         

-- interface Genem2 x y z (f : x -> y -> z) p1 p2 p3 | f,x,y,p1,p2 where
--   lift : (xv : Prv x p1) -> (yv : Prv y p2)-> Prv z (f xv yv)

-- data Genem2 : (f : x -> y -> z) -> (p1 : x -> Bool) -> (p2 : y -> Bool) -> (f xv yv -> Bool) where
--   [search x,y,f,p1,p2]
--   MkGenem2 

-- Genem2 (/) (> 0) (> 0) (> 0) where
--   lift f px py = ?xx


data Fook v = Fee | Fooz v

Show v => Show (Fook v) where
  show Fee = "Fee"
  show (Fooz x) = "Fooz " ++ show x

[noshowv] Show (Fook v) where
  show Fee = "Fee"
  show (Fooz x) = "Fooz"


test5 : String
test5 = let x : Fook String
            x = Fooz "foo"
        in show x
        

data Noprint = No1

test6 : String
test6 = let x : Fook Noprint
            x = Fooz No1
        in show @{noshowv} x
