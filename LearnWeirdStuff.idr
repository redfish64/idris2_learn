module LearnWeirdStuff

import public Text.Lexer
import public Text.Parser
import Data.Nat
import Data.Vect
import Data.List as L
import Data.So

%default total

namespace A
  public export
  Rule : Type -> Type -> Type
  Rule token ty = Grammar (TokenData token) True ty
  Rule' : Type -> Type -> Type
  Rule' token = Grammar (TokenData token) True 


data Token
  = Comment String
  | EndOfInput
  | Equals
  | DotSepIdent (List String)
  | Separator
  | Space
  | StringLit String

Rule : Type -> Type
Rule = Rule Token --this takes Rule from A


-- data Foo : Int -> Type where
--   MkInitFoo : (v : Int) -> Foo v
--   MkFoo : (v : Int) -> (Foo _) -> Foo v2
  
data Foo : Int -> a -> Type where
  MkInitFoo : (v : a) -> Foo 0 a
  --MkFoo : (0 ck : Int) -> (0 p : Foo pk _ ) -> (v : a) -> Foo ck a
  MkFoo' : (0 ck : Int) -> (0 p : Foo pk whyNeedMe ) -> (v : a) -> Foo ck a
  MkFoo'' : (0 p : Foo pk _ ) -> (0 ck : Int) -> (v : a) -> Foo ck a


namespace X1 
  export 
  foo : Int -> Int
  foo = (+ 1)
  
namespace X2 
  export 
  foo : String -> String
  foo = (++ "bar")
 
test1 : Int
test1 = foo 5
 
test2 : String
test2 = foo "foo"
 
  
  
axes : (n : Nat) -> {auto gt : GT n 0} -> {auto lte : LTE n 4} -> Vect n String
axes 1 {lte = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))} impossible
axes 2 {lte = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))} impossible
axes 3 {lte = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))} impossible
axes 4 {lte = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))} impossible
axes (S (S (S (S (S _))))) {lte = LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))} impossible
--axes (S (S (S (S (S _))))) {lte = (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _ )))))} impossible


--home grown variable number of args
CreateFuncType : (n : Nat) -> (v : Type) -> (ret : Type) -> Type
CreateFuncType 0 v ret = ret
CreateFuncType (S k) v ret = (v -> CreateFuncType k v ret)

applyVectToFunc : Vect n v -> CreateFuncType n v x -> x
applyVectToFunc [] f = f
applyVectToFunc (v :: xs) f = applyVectToFunc xs (f v)

test3 : Int
test3 = applyVectToFunc [1,2] (+)


test4 : (x : Bool) -> (y : case x of True => Int; False => Char) -> Bool
test4 x y = False

data CrunchTypes : Type -> Type -> Type -> Type where
  MkCrunch : CrunchTypes x x Nat
  MkCrunch' : CrunchTypes x y Bool

test5 : (x : t1) -> (y : t2) -> {auto z : CrunchTypes t1 t2 ct} -> ct -> Bool
test5 x y c = False

test6 : Bool
test6 = test5 1 2 Z

test7 : Bool
test7 = test5 1 '2' True

data CT2Prop : Nat -> Nat -> Type where
  CT2P1 : CT2Prop Z Z
  CT2P2 : CT2Prop x y -> CT2Prop x (S y)

data CrunchTypes2 : (t1 : Type) -> (t2 : Type) -> (v1 : t1) -> (v2 : t2) -> Type -> Type where
  MkCrunch2 : {auto 0 prf : CT2Prop v1 v2} -> CrunchTypes2 Nat Nat v1 v2 Int
  --MkCrunch2' : Ord x => (So (v1 <= v2)) -> CrunchTypes2 x x v1 v2 Int

test8 : (x : t1) -> (y : t2) -> {auto z : CrunchTypes2 t1 t2 x y ct} -> ct -> Bool
test8 x y ct = False

test9 : Bool
test9 = test8 Z (S (S (S Z))) (the Int 3)


namespace Foo46
  export
  myBind : (Int -> Int) -> (() -> Int -> Int) -> (Int -> Int)
  myBind f g x = g () (f x)
  
  export
  (>>=) : (Int -> Int) -> (() -> Int -> Int) -> (Int -> Int)
  (>>=) = myBind

test : Int
test =
  -- ((+ 5) >>= (+ 7)) 0
  -- let (>>=) = Foo46.myBind
  -- in 
   (Foo46.do
       (+ 5)
       (+ 7)
       (+ 9)) 7
    


namespace Foo47
  export
  myBind : Int -> (() -> Int) -> Int
  myBind f g = g () + f
  
  export
  (>>=) : Int -> (() -> Int) -> Int
  (>>=) = myBind

test10 : Int
test10 =
  -- let (>>=) = Foo47.myBind
  -- in
  --   5 >>= (\_ => 7) >>= (\_ 9)1
  
  -- ((+ 5) >>= (+ 7)) 0
  -- in 
   (Foo47.do
       5
       7
       9) 

data IsTypesEqual : Type -> Type -> Type where
  Equal : IsTypesEqual x x 
  NotEqual : IsTypesEqual x y 

data ITEWrapper : IsTypesEqual x y -> Type where
  WEq : ITEWrapper Equal
  WNE : ITEWrapper NotEqual

foo : (v1 : t1) -> (v2 : t2) -> {auto 0 te : IsTypesEqual t1 t2} -> {auto weq : ITEWrapper te} -> Int
foo v1 v2 {te = Equal} {weq = WEq} = 0
foo v1 v2 {te = NotEqual} {weq = WNE} = 1

foo2 : (v1 : t1) -> (v2 : t2) -> {auto te : IsTypesEqual t1 t2} -> Int
foo2 v1 v2 {te = Equal} = 0
foo2 v1 v2 {te = NotEqual} = 1

test11 : Int
test11 = foo "x" 5

test12 : Int
test12 = foo 5 6

test13 : Int
test13 = foo2 "x" 5

test14 : Int
test14 = foo2 5 6

--namespace AUTOBIND
-- data Color = Blue | Green | Red | Uno

-- data Card : Color -> Type where
--   MkCard : (x : Color) -> Card x
--   MkUnoCard : Card Uno -> Card w --uno card can be anything

-- data Match : Color -> Color -> Color -> Type where
--   MkMatch : (x : Color) -> Match x x x
--   MkUno1 : Match Uno y z
--   MkUno2 : Match x Uno z
--   MkUno3 : Match x y Uno
    
-- ComputeBind : Match x y z -> Type
-- ComputeBind (MkMatch z) = (Card x -> Card x)
-- ComputeBind MkUno1 = (Card x -> Card z)
-- ComputeBind MkUno2 = (Card x -> Card z)
-- ComputeBind MkUno3 = ({m : Color} -> Card x -> Card m)

-- (>>=) : ((Card xc) -> (Card yc)) -> (() -> Card yc -> Card zc) -> {auto match : Match xc yc zc} -> ComputeBind match
-- (>>=) f g {match = (MkMatch Uno)} = g () . f
-- (>>=) f g {match = (MkMatch _)} = g () . f
-- (>>=) f g {match = MkUno1} = g () . f
-- (>>=) f g {match = MkUno2} = g () . f
-- (>>=) f g {match = MkUno3} = MkUnoCard . g () . f

namespace AUTOBIND
  data Card = Blue | Green | Red | Uno

  data Match : Card -> Card -> Type where
    MkMatch : (x : Card) -> Match x x
    MkUno : Match Uno y
    MkUno2 : Match x Uno

  data Pile : (firstCard : Card) -> (lastCard : Card) -> Type where
    MkPile : (x : Card) -> Pile x x
    ConcatPiles : Pile w x -> Pile y z -> {0 m : Match x y} -> Pile w z

  (>>=) : (Pile f1 l1) -> (() -> Pile f2 l2) -> {auto 0 match : Match l1 f2} -> Pile f1 l2
  (>>=) p1 f = ConcatPiles {m=match} p1 $ f ()
  
  card : (x : Card) -> Pile x x
  card x = MkPile x

  test15 : Pile Red Uno 
  test15 =
    AUTOBIND.do
      card Red
      card Red
      card Uno
      card Green 
      card Green 
      card Uno
      card Blue 
      card Uno
 
foo3 : x -> y -> {auto 0 prf : x = y} -> x
foo3 x y = x

test16 : Int
test16 = foo3 3 6

foo4 : x -> y -> {auto prf : Either (x = y) (x = Int)} -> x
foo4 x y {prf=(Left xeqy)} = x
foo4 x y {prf=(Right xIsInt)} = rewrite xIsInt in 5

test17 : Int
test17 = foo4 6 8

test18 : Int
test18 = foo4 6 'x'

namespace AUTOBIND2
  data Card : (deck : Nat) -> Type where
    Blue : Card x 
    Green : Card x
    Red : Card x
    Uno : Card x

  data Match : Card d -> Card d -> Type where
    MkMatch : (x : Card d) -> Match x x
    MkUno : Match Uno y
    MkUno2 : Match x Uno

  public export
  data Match2 : Card d1 -> Card d2 -> Type where
    MkMatch2 : Match2 Uno Uno
    
  -- data Pile : (firstCard : Card d1) -> (lastCard : Card d2) -> Type where
  --   MkPile : (x : Card) -> Pile x x
  --   ConcatPiles : Pile w x -> Pile y z -> {0 m : Match x y} -> Pile w z

  -- export
  -- (>>=) : (Pile f1 l1) -> (() -> Pile f2 l2) -> {auto 0 match : Match l1 f2} -> Pile f1 l2
  -- (>>=) p1 f = ConcatPiles {m=match} p1 $ f ()
  
  -- export
  -- card : (x : Card) -> Pile x x
  -- card x = MkPile x

data LessThanForProof : (x : xt) -> (y : yt) -> (prf : xt = yt) -> Type where
  Dummy : LessThanForProof x y prf

pairFun : Num xt => (x : xt) -> (y : yt) -> (prf : xt = yt) -> {auto prf2 : LessThanForProof x y prf} -> xt
pairFun x y prf = x

testPairFun : Nat
testPairFun = pairFun Z (S Z) Refl

dummyMaybe : {auto x : Maybe Int} -> Maybe Int
dummyMaybe {x} = x

-- dummyMaybe2 : (x : xt) -> (xt = Maybe Int) -> Maybe Int
-- dummyMaybe2 x {xt} prf = 
--    let prf2 = rewrite prf in xt

-- data Foo18 = Apple | Pear | Banana

-- data Foo19 : Type where
--   Fee =  

-- namespace COBJ
--   public export
--   data CObj = Init | Name | CName | Abs | Cnstr | Method | Fin

--   MoveCObj = mkMetaSpec $ 
--     do
--       Init
--       Name <|> CName
--       Abs <|> Cnstr
--       Star Method
--       Fin
      
  -- data MoveNameCName : CObj -> CObj -> Type where
  --   N1 : MoveNameCName Init (Name _)
  --   N2 : MoveNameCName (Name _) (CName _)
  --   N3 : MoveNameCName (Name _) Fin
  --   N4 : MoveNameCName (CName _) Fin

  -- public export
  -- data MoveCObj : CObj -> CObj -> Type where
  --   M1 : MoveNameCName Init x -> MoveCObj Init x
  --   M1_1 : MoveNameCName x y -> MoveCObj x y
  --   M1_2 : MoveNameCName x Fin -> MoveCObj x Abs
  --   M5 : MoveCObj Abs Fin
  --   M6 : MoveCObj Abs (Method _)
  --   M7 : MoveCObj (Name _) (Cnstr _)
  --   M8 : MoveCObj (CName _) (Cnstr _)
  --   M9 : MoveCObj (Cnstr _) Fin
  --   M11 : MoveCObj (Cnstr _) (Method _)
  --   M12 : MoveCObj (Method _) (Method _)
  --   M13 : MoveCObj (Method _) Fin

      
-- data Unique : (a : Type) -> (p : a -> Type) -> Type where
--     MkUnique : (x : a) -> p x -> ((y : a) -> p y -> (x = y)) 
--                -> Unique a p

-- uniqueZero : Unique Nat (\n => n + n = n) 
-- uniqueZero = MkUnique Z Refl ?ppp2 

-- data UDPair : DPair a p -> Type where
--   MkUDPair : {a : Type} -> {p : a -> Type} -> (dp : DPair a p) -> ((y : a) -> p y -> (fst dp) = y)
--   where
--     MkUnique : (x : a) -> p x -> ((y : a) -> p y -> (x = y)) 
--                -> Unique a p

data Unique : (a : Type) -> (p : a -> Type) -> Type where
    MkUnique : (x : a) -> p x -> ((y : a) -> p y -> (x = y)) 
               -> Unique a p

Unique' : (x : Type) -> Type
Unique' (DPair a p) = Unique a p
Unique' _ = Void

x : Int -> True = False -> Void
x y Refl impossible

-- sxEqSyToXEqY : {x : Nat} -> {y : Nat} -> S x = S y -> x = y
-- sxEqSyToXEqY {x = 0} {y = 0} prf = Refl
-- sxEqSyToXEqY {x = (S k)} {  y = 0} Refl impossible
-- sxEqSyToXEqY {x = 0} {y = S k} Refl impossible
-- sxEqSyToXEqY {x = S j} {y=S k} prf = ?sxEqSyToXEqY_rhs_

-- itsImpossible : {k : Nat} -> (S k + S k) = (S k) -> Void
-- itsImpossible prf = ?xxx 

-- allZeroPrf : (y : Nat) -> y + y = y -> 0 = y
-- allZeroPrf 0 prf = Refl
-- allZeroPrf (S k) prf = void $ itsImpossible prf

-- uniqueZero : Unique' (n : Nat ** n + n = n)
-- uniqueZero = MkUnique Z Refl allZeroPrf

--uniqueZero : UDPair (n : Nat ** ((n + n) = n))
--uniqueZero : UDPair (n : Nat ** ((n + n) = n))

-- uniqueZero2 : UDPair (DPair Nat (\n => (n + n) = n))
--uniqueZero = MkUnique Z Refl ?ppp2 

-- data Unique' : (a : Type) -> {x : a} -> {p : a -> Type} -> p x -> Type where
--     -- MkUnique' : (x : a) -> p {av=x} -> ((y : a) -> p -> (x = y)) 
--     --            -> Unique' a p

-- uniqueZero' : Unique' Nat {x=Z} ((n + n) = n)
-- uniqueZero' = MkUnique' Z ?ppp' ?ppp2' 

--uniqueZero : Unique (n : Nat) (\n => n + n = n) 

data XIsAcceptable : Type -> Type where
  IsInt : XIsAcceptable Int
  IsChar : XIsAcceptable Char
 
Foo5 : {x : Type} -> {auto 0 prf : XIsAcceptable x} -> Maybe x
Foo5 = Nothing

test19 : Maybe Int
test19 = Foo5

-- doesn't work
-- test20 : Maybe String
-- test20 = Foo5

--we can do the same thing with binds

||| a list of types that have to be in a type family (called c) 
data ListTF : (c : Type -> Type) -> Type where
  Nil : ListTF c
  Cons : c v -> ListTF c -> ListTF c

-- foo6 : (myTF : Type -> Type) -> (v : myTF vt) -> (bl : ListTF myTF -> Type -> Type) -> (bl Nil vt)  -> ListTF myTF -> myTF Int

-- foo7 : Int
-- foo7 = foo7


namespace A
  export
  foo6 : Int -> Int
  foo6 = (+ 42)

namespace B 
  export
  foo6 : Int -> Int -> Int
  foo6 = (+)

testfoo6 : Int
testfoo6 = foo6 6

  
-- data Stuff : Int -> Type where
--   MkStuff = { v : Int } -> v = 

-- foo7 : {0 x : Bool} -> Int
-- foo7 {x=True} = 1
-- foo7 {x=False} = 0

-- foo8 : {0 x : Bool} -> {y : Bool} -> Int
-- foo8 {x=True} {y=x} = 1
-- foo8 {x=False} {y=x} = 0

-- foo9 : {0 x : Bool} -> (if x then Int else Char)
-- foo9 {x = True} = ?foo9_rhs_1
-- foo9 {x = False} = ?foo9_rhs_

interface Foo7 v where
  repSample : v a -> a
  
-- Foo7 (Vect n) where
--   repSample : {auto k : Nat} -> Vect (S k) v -> v
  
  
-- letMatchTest : Bool -> Bool
-- letMatchTest y =
--  let
--    X : Bool
--    X = True
--  in
--    case y of
--      X => True
--      _ => False

test20 : List (Int, Int)
test20 = L.filter (uncurry (/=)) [ (a, b) | a <- [1,10], b <- [1..10]]

test21 : Int -> Int -> Int
test21 = (\x,y => x + y)


test22 :  Int
test22 = 
  let v : List Int -> Int
      v (x :: xs) = x + v xs
      v [] = 0
  in v [1,2,3]
  
  
test23 : Int
test23 = foo
  where 
    foo : Int
    foo = fee
      where 
        fee : Int
        fee = 42

test24 : Int -> List Int
test24 v = (map id [1..v]) ++ (map id [1..v])

sl : Int -> String
sl v = "l" ++ show v

sr : Int -> String
sr v = "r" ++ show v

test25 : Int -> List String
test25 v = (map sl [1..v]) ++ (map sr [1..v])

-- test25 : Int
--   let x = [0..test24]
  

foo26 : {at : Type} -> {bt : Type} -> (a : at) -> (b : bt) -> {auto prf : at = bt} -> Int
foo26 a b = 42

test27 : Int
test27 = foo26 () ()

test28 : Int
test28 = foo26 String String

-- test29 : Int
-- test29 = foo26 (the (Vect 2 Type) [String,Int]) (the (Vect 2 Type) [String,Int]


-- data Foo30 : List Type -> Type where
--   MkFoo30 : (key : kt) ->

data Foo31 : Type where
  ||| what is a neighbor?
  Neighbor : Foo31
  ||| or a stranger?
  Stranger : Foo31
 
test29 : 1=1
test29 = Refl

-- test30 : Bool -> Bool
namespace T30
  test30 : Int
  test30 = 42
  
namespace T31
  test30 : String
  test30 = "foo"
 
t32 : Type 
t32 = {kind : _} -> (a ** kind a => a -> a)

t33 : Type 
t33 = {a : Type } -> {kind : _} -> (a ** kind a => a -> a)

t34 : Type 
t34 = {kind : _ } -> {a : Type} -> (a ** kind a => a -> a)

-- t35 : Bool -> Maybe Int -> Bool
-- t35 x Nothing = ?zzx
-- t35 x (Just y) = x


-- t35 True = ?yyy_1
-- t35 False = ?yyy_

-- %logging interaction 3
-- %logging tim 5


namespace T36
  -- LOG interaction.casesplit:3: Splitting: (pat x : Prelude.Basics.Bool) => (LearnWeirdStuff.T36.t36 x[0])
  --   (Bind _ x (PVar _ c info Prelude.Basics.Bool) (App _ (Ref LearnWeirdStuff.T36.t36) (Local _ _ 0 _)))
  -- LOG interaction.casesplit:3: vars [x]
  -- LOG interaction.casesplit:3: fn tyn cons LearnWeirdStuff.T36.t36 Prelude.Basics.Bool [Prelude.Basics.True, Prelude.Basics.False]
  -- LOG interaction.casesplit:3: rawlhs (LearnWeirdStuff.T36.t36 x)
  -- LOG interaction.casesplit:3: trycases [(LearnWeirdStuff.T36.t36 Prelude.Basics.True), (LearnWeirdStuff.T36.t36 Prelude.Basics.False)]
  -- LOG interaction.casesplit:3: idx 300
  -- LOG interaction.casesplit:3: cases [Invalid, Invalid]
  -- export -- with export it works fine
  
  -- error is:
  -- LOG interaction.casesplit:3: Err: :1:1--1:1:Name LearnWeirdStuff.T36.t36 is private
  -- LOG interaction.casesplit:3: Err: :1:1--1:1:Name LearnWeirdStuff.T36.t36 is private
  -- t36 : Bool -> Bool
  -- t36 True = ?zz_1
  -- t36 False = ?zz_
  -- t36 True = ?xxx_1
  -- t36 False = ?xxx_

-- LOG interaction.casesplit:3: Splitting: (pat x : Prelude.Basics.Bool) => (LearnWeirdStuff.t37 x[0])
-- LOG interaction.casesplit:3: vars [x]
-- LOG interaction.casesplit:3: fn tyn cons LearnWeirdStuff.t37 Prelude.Basics.Bool [Prelude.Basics.True, Prelude.Basics.False]
-- LOG interaction.casesplit:3: rawlhs (LearnWeirdStuff.t37 x)
-- LOG interaction.casesplit:3: trycases [(LearnWeirdStuff.t37 Prelude.Basics.True), (LearnWeirdStuff.t37 Prelude.Basics.False)]
-- LOG interaction.casesplit:3: idx 301
-- LOG interaction.casesplit:3: Original LHS: (LearnWeirdStuff.t37 x)
-- LOG interaction.casesplit:3: New LHS: (LearnWeirdStuff.t37 Prelude.Basics.True)
-- LOG interaction.casesplit:3: Original LHS: (LearnWeirdStuff.t37 x)
-- LOG interaction.casesplit:3: New LHS: (LearnWeirdStuff.t37 Prelude.Basics.False)
-- LOG interaction.casesplit:3: cases [Valid: (LearnWeirdStuff.t37 Prelude.Basics.True)
-- Updates: [(x, Prelude.Basics.True), (LearnWeirdStuff.t37, LearnWeirdStuff.t37)], Valid: (LearnWeirdStuff.t37 Prelude.Basics.False)
-- Updates: [(x, Prelude.Basics.False), (LearnWeirdStuff.t37, LearnWeirdStuff.t37)]]
-- t37 : Bool -> Bool
-- -- t37 x = ?xxx --original, before case split
-- t37 True = ?xxy_1
-- t37 False = ?xxy_

interface Foo38 x where
  foo38 : x -> Int

mutual 
  Foo38 String where
    foo38 x = 42

  -- foo39 : Bool -> Int

foo40 : Show v => v -> String
foo40 va = foo40' va
  where
    foo40' : v -> String
    foo40' va1 = show (the (List v) (va :: va1 :: []))

--%logging tim 5
      
updateEntry : (v -> Bool) -> v -> (v -> v) -> List v -> List v
updateEntry fn defVal updFn l = updateEntry1 l
  where
    updateEntry1 : List v -> List v
    updateEntry1 [] = [defVal]
    updateEntry1 (x :: xs) = []
      -- case idFn x of 
      --   True => updVal x :: xs
      --   False => x :: updateEntry1 defVal xs

t41 : (Int, Char, String)
t41 = (5,'c',"foo")

t42 : Maybe (Maybe Int) -> Maybe Int
t42 v = do
          (Just x) <- v
            | Nothing => Nothing
          pure x
          

  
t43 : Int
t43 = 42

t44 : Int
t44 = t43

t45 : Int
t45 = t44


foo46 : Bool -> Int
foo46 True = t43
foo46 False = 0

foo47 : Bool -> Int
foo47 y = (\x => the Int $ case x of 
                   True => t43
                   False => the Int 0) y

foo48 : Bool -> Int
foo48 y = (\x => t43) y

foo49 : Bool -> Int
foo49 x = case x of 
            True => t43
            False => the Int 0

data Foo50 : Type -> Type where
  MkFoo50 : (x : v) -> Foo50 v

test51 : Foo50 v -> v
test51 (MkFoo50 x) = x

test52 : Foo50 t -> t
test52 v = doit v
  where 
    doit : Foo50 t' -> t'
    doit (MkFoo50 x) = x
   
-- Callers
-- - + LearnWeirdStuff.Foo50
--  |[+] LearnWeirdStuff.MkFoo50
--  |[+] LearnWeirdStuff.MkFoo53
--  |-- LearnWeirdStuff.Foo53.bar
--  |-- LearnWeirdStuff.Foo53.bee
--  |-- LearnWeirdStuff.test51
--  |-- LearnWeirdStuff.test52
--  |-- LearnWeirdStuff.Foo53.(.bar)
--  `-- LearnWeirdStuff.Foo53.(.bee)
record Foo53 where
  constructor MkFoo53
  bar : Foo50 Int
  bee : Char

interface Foo54 x where
  foo54 : x -> Int
  
-- Callers (of Foo54)
-- - + LearnWeirdStuff.Foo54
--  |-- LearnWeirdStuff.foo54
--  |-- LearnWeirdStuff.Foo54 implementation at LearnWeirdStuff.idr:701:1--705:1
--  `-- LearnWeirdStuff.Foo54 at LearnWeirdStuff.idr:693:1--701:6
Foo54 (Int -> Int) where
  foo54 x = 42
  
  
   
