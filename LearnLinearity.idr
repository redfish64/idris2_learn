import Data.Vect as V

--%default total

-- This is my own idea, a List of things that each can only be referenced once, therefore
-- it is a set of unique items as well

--items should be linear
data LinList : (vt : Type) -> Type where
  LNil : LinList vt
  LCons : (1 v : vt) -> LinList vt -> LinList vt
  
--testing it out
test : LinList Int
test = let s = 42
           x = LNil
           x1 = LCons s x
           x2 = LCons s x1
       in x2
       
-- ok doesn't work

--here we accept a linear Int
test1 : (1 s : Int) -> LinList Int
test1 s = let
           x = LNil
           x1 = LCons s x
           --x2 = Cons s x1
          in x1
       

-- won't work with a standard list. Makes sense
-- test2 : (1 s : Int) -> List Int
-- test2 s = let
--            x = []
--            x1 = s :: x
--            --x2 = Cons s x1
--           in x1


--taken from Data.Vect
data Vect1 : (len : Nat) -> (elem : Type) -> Type where
  ||| Empty vector
  VNil  : Vect1 Z elem
  ||| A non-empty vector of length `S len`, consisting of a head element and
  ||| the rest of the list, of length `len`.
  VCons : (1 x : elem) -> (1 xs : Vect1 len elem) -> Vect1 (S len) elem

test2 : (1 s1 : Int) -> Vect1 2 Int
test2 s1 = let x = VNil
               x1 = VCons s1 x
               x2 = VCons 42 x1
           in x2
           
plus1 : (1 x : Int) -> (1 y : Int) -> Int
plus1 x y = (assert_linear (assert_linear (+) x) y)

addInts : (1 vs : Vect1 n Int) -> Int
addInts VNil = 0
addInts (VCons x xs) = plus1 x $ addInts xs

test3 : (1 s1 : Int) -> Int
test3 s1 = let x = VNil
               x1 = VCons s1 x
               x2 = VCons 42 x1
           in addInts x2


record Foo where
  constructor MkFoo
  dat : Int
  1 lin : Int


getit : Foo -> Int
getit (MkFoo dat lin) = dat

data L2Pair : Type -> Type -> Type where
  MkL2Pair : vt1 -> (1 v2 : vt2) -> L2Pair vt1 vt2

test4 : (1 f : Foo) -> L2Pair Int Foo
test4 (MkFoo dat lin) = MkL2Pair dat (MkFoo dat lin)

rep : {n : Nat} -> a -> Vect n a
rep { n = Z } val = []
rep { n = S k } val = val :: rep val

namespace LinListFun
  --items should be linear
  data LList : (vt : Type) -> Type where
    LNil : LList vt
    LCons : (1 v : vt) -> LList vt -> LList vt
  
  toList : LList v -> List v
  toList LNil = Nil
  toList (LCons x y) = x :: toList y

  --takes a list of v and a function f and guarantees that f doesn't produce a list with duplicate items from its input
  makeUniqueSet : List v -> (f : (1 _ : v) -> LList v -> LList v) -> List v
  makeUniqueSet xs f = let x = foldr (\e,acc => f e acc) LNil xs
                       in toList x

  --add to head
  testUSF1 : (1 _ : Int) -> LList Int -> LList Int
  testUSF1 x xs = x `LCons` xs
  
  namespace V1
    --append linlists
    appendLL : LList x -> LList x -> LList x
    appendLL LNil z = z
    appendLL (LCons x xs) ys = LCons x (xs `appendLL` ys)

    --reverse a linlists (kinda slow)
    reverseLL1 : LList x -> LList x
    reverseLL1 LNil = LNil
    reverseLL1 (LCons x xs) = (reverseLL1 xs) `appendLL` (x `LCons` LNil)

    reverseLL2Helper : (inp : LList x) -> (acc : LList x) -> LList x
    reverseLL2Helper LNil acc = acc
    reverseLL2Helper (LCons x xs)  acc = reverseLL2Helper xs (x `LCons` acc)

    reverseLL2 : LList x -> LList x
    reverseLL2 y = reverseLL2Helper y LNil

namespace LinListFun2
  --items should be linear
  data LList : (vt : Type) -> Type where
    LNil : LList vt
    LCons : (1 v : vt) -> (1 _ : LList vt) -> LList vt

  --append linlists
  appendLL : (1 _ : LList x) -> (1 _ : LList x) -> LList x
  appendLL LNil z = z
  appendLL (LCons x xs) ys = LCons x (xs `appendLL` ys)

  --reverse a linlists (slow)
  reverseLL1 : (1 _ : LList x) -> LList x
  reverseLL1 LNil = LNil
  reverseLL1 (LCons x xs) = (reverseLL1 xs) `appendLL` (x `LCons` LNil)

  reverseLL2Helper : (1 inp : LList x) -> (1 acc : LList x) -> LList x
  reverseLL2Helper LNil acc = acc
  reverseLL2Helper (LCons x xs)  acc = reverseLL2Helper xs (x `LCons` acc)

  --faster version
  reverseLL2 : (1 _ : LList x) -> LList x
  reverseLL2 y = reverseLL2Helper y LNil

  testUSF2 : (1 x : Int) -> (1 _ : LList Int) -> LList Int
  testUSF2 x xs = reverseLL2 $ x `LCons` xs

  -- testFun : (
  
  data LNat : Type where
    LZ : LNat
    LS : (1 _ : LNat) -> LNat
  
  count : LList xt -> LNat
  count LNil = LZ
  count l@(LCons v x) = LS (count x)

  --won't work, because v is used zero times 
  -- count' : (1 _ : LList xt) -> LNat
  -- count' LNil = LZ
  -- count' (LCons v xs) = LS (count' xs)
 
  --we store v up in a result that we give (like a white elephant, the caller still must process the v's since
  --they are linear)
  count'' : (1 _ : LList xt) -> (LPair (LList xt) LNat)
  count'' LNil = (LNil # LZ)
  count'' (LCons v xs) = 
     let (l2 # cnt) = count'' xs
     in (LCons v l2 # LS cnt)
 
 
  -- testCount : (1 _ : LList xt) -> LNat
  -- testCount l = let c = count' l
  --               in ?xxx
  
-- testUS1 : ()
-- testUS1 = let inp = [1,2,3,4,5]
--               f1 
-- namespace Try2
--   --items should be linear
--   data LList : (vt : Type) -> Type where
--     LNil : LList vt
--     LCons : (1 v : vt) -> LList vt -> LList vt
    
--   --takes a list of v and a function f and guarantees that f doesn't produce a list with duplicate items from its input
--   makeUniqueSet : List v -> (f : (1 _ : v) -> (1 _ : LList v) -> LList v) -> List v
--   makeUniqueSet xs f = let x = foldr (\e,acc => f e acc) LNil xs
--                        in toList x

--   --add to head
--   testUSF1 : (1 _ : Int) -> (1 _ : LList Int) -> LList Int
--   testUSF1 x xs = x `LCons` xs

--   -- --append linlists
--   -- appendLL : LList x -> LList x -> LList x
--   -- appendLL LNil z = z
--   -- appendLL (LCons x xs) ys = LCons x (xs `appendLL` ys)

--   -- --reverse a linlists (kinda slow)
--   -- reverseLL1 : LList x -> LList x
--   -- reverseLL1 LNil = LNil
--   -- reverseLL1 (LCons x xs) = (reverseLL1 xs) `appendLL` (x `LCons` LNil)

--   -- reverseLL2Helper : (inp : LList x) -> (acc : LList x) -> LList x
--   -- reverseLL2Helper LNil acc = acc
--   -- reverseLL2Helper (LCons x xs)  acc = reverseLL2Helper xs (x `LCons` acc)

--   -- reverseLL2 : LList x -> LList x
--   -- reverseLL2 y = reverseLL2Helper y LNil

--   -- -- doesn't work, see we're leaking elements. v is destroyed
--   -- testUSF2 : (1 x : Int) -> LList Int -> LList Int
--   -- testUSF2 x LNil = LCons x LNil
--   -- testUSF2 x (LCons v xs) = LCons x xs

-- -- testUS1 : ()
-- -- testUS1 = let inp = [1,2,3,4,5]
-- --               f1 

