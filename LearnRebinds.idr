module LearnRebinds

import Control.Monad.Trans
import Data.List

(>>=) : IO a -> (a -> IO b) -> IO b
(>>=) = Prelude.(>>=)

test : IO ()
test = let (>>=) = LearnRebinds.(>>=)
       in 
         do 
           putStrLn "foo!"
   
-- test2 : Monad m => m Int
-- test2 = let (>>=) = CSpecBuilder.(>>=)
--         in 
--           do
--             v <- pure 5 
--             pure $ v + v

test3 : Monad m => m Int
test3 = do
           v <- pure 5 
           pure $ v + v
           
--limit steps bind

namespace LSM
  public export
  interface LSMonad (m : Nat -> Type -> Type) where
    pure : {0 remSteps : Nat} -> v -> m remSteps v 
    (>>=) : {0 remSteps' : Nat} -> m (S remSteps') a -> (a -> m remSteps' b) -> m (S remSteps') b

data LimitStepsMonad : (remSteps : Nat) -> (vt : Type) -> Type where
  MkLSM : {0 remSteps : Nat} -> vt -> LimitStepsMonad remSteps vt

LSMonad LimitStepsMonad where
  (>>=) m@(MkLSM v1) fn = let (MkLSM vres) = fn v1 
                          in MkLSM vres
  pure v = MkLSM v

foo : LSMonad m => m 6 Int
foo = pure 6

--maximum steps of 5
foo2 : LSMonad m => m 5 Int
foo2 =
    --it needs these two withs, or it will get confused and use Prelude.pure
    with LSM.(>>=) 
    with LSM.pure
    do 
      v1 <- pure 5
      v2 <- pure 6
      pure 6
      pure 6
      pure 6 -- this can be commented out, because we don't need to
             -- end with zero, so we can allow less than 5 steps 
      -- pure 6 --will fail with a 6th step
      pure (v1 + v2)

--required and optional parameters as a do statement

namespace PNT
  -- public export
  -- data Compat : (x : Type -> Type) -> Type where
  --   MkCompat : (compat : x t1 -> x t2 -> Type) -> Compat x
    
  -- compat : {auto com : Compat x} -> (x t1) -> (x t2) -> Type
 
  public export
  data Compat : (v : Type) -> (x : v -> Type) -> Type where
    MkCompat : (compat_ : (t1 : v) -> (t2 : v) -> Type) -> Compat v x
    --MkCompat : (compat : x t1 -> x t2 -> Type) -> Compat x
    
  compat : {v : Type} -> (x : v -> Type) -> {auto com : Compat v x} -> (t1 : v) -> (t2 : v) -> Type
  compat {v=v1} x {com = MkCompat compatInner} = compatInner 
  
  -- -- public export
  -- -- interface Compat (v' : Type) (x' : v' -> Type) where
  -- --   compat : {v : Type} -> (x : v -> Type) -> (t1 : v) -> (t2 : v) -> Type
  
  public export
  data Composite : List Type -> Type where
       CompNil : Composite []
       CompCons : (x : ty) -> Composite tys -> Composite (ty :: tys)
       
  -- len : Composite x -> Nat
  -- len CompNil = 0
  -- len (CompCons x xs) = S (len xs)
       
  -- data FooNat : Nat -> Type where
  --   MkFN : (x : Nat) -> FooNat x
  
  %hint CompatComposite : Compat (List Type) Composite 
  CompatComposite = MkCompat compat_meth where
     compat_meth : (t1 : List Type) -> (t2 : List Type) -> Type
     compat_meth t1 t2 = (length t1 = length t2)
     
  foo : (x: Composite [Int , String]) -> (y : Composite [Char]) -> compat Composite [] []
  foo _ _ = Refl
     
  -- Compat Nat FooNat where
  --    compat (MkFN n1) (MkFN n2) = (n1 = (S n2))
    
  -- -- foo : Compat n x => {v1 : n} -> {v2 : n} -> (xv1 : x v1) -> (xv2 : x v2) -> compat {t1=v1} {t2=v2} xv1 xv2

  -- public export
  -- interface PNTMonad (m : Type -> Type -> Type) where
  --   pure : v -> m t v 
  --   (>>=) : Compat v x => {t1 : v} -> {t2 : v} -> {xp : compat t1 t2} -> m (x t1) a -> (a -> m (x t2) b) -> m (x t2) b
    
-- data ParamType : Type where
--   RequiredParam : ParamType
--   OptionalParam : ParamType

-- data Param : (paramNameType : Type) -> ParamType -> (used : Bool) -> Type where
--   MkParam : (nm : paramNameType) -> (pt : ParamType) -> Param paramNameType pt False 
--   UseParam : (v : Param paramNameType pt False) -> Param paramNameType pt True

-- data Params : (paramNameType : Type) -> Type where
--   MkParams : List (paramNameType,ParamType) -> Params paramNameType

-- data Param : (paramNameType : Type) -> ParamType -> (used : Bool) -> Type where
--   MkParam : (nm : paramNameType) -> (pt : ParamType) -> Param paramNameType pt False 
--   UseParam : (v : Param paramNameType pt False) -> Param paramNameType pt True

-- pname : Param pnt _ _ -> pnt
-- pname (MkParam nm _) = nm
-- pname (UseParam v) = pname v

-- namespace P
--   public export
--   interface ParamsMonad (m : 
-- foo3 : ParamMonad [ 

data Param : Type where
  AbstractFlag : Param
  Name : Param
  CName : Param
  Cnstr : Param

data ParamInUse : Param -> Type where
  MkPIU : (x : Param) -> ParamInUse x


namespace FMT
  
  public export
  interface Monad1 (m : Type -> Type) where
    pure : (1 _ : a) -> m a
    (>>=) : (1 act : m a) -> (1 k : (1 _ : a) -> m b) -> m b
    

data Id : Type -> Type where
  MkId : (1 _ : x) -> Id x
 
runId : Id v -> v 
runId (MkId v) = v

Monad1 Id where
  pure v = MkId v
  (>>=) (MkId v) f = f v

-- createObj : (
--             -- {1 absFlag : ParamInUse AbstractFlag} -> 
--             -- {1 name : ParamInUse Name} -> 
--             (1 cname : ParamInUse CName) -> 
--             (1 cnstr : ParamInUse Cnstr) -> Id ())-> ()
-- createObj i = runId (i (MkPIU Cname Cnstr))

-- mkCname : (1 cname : ParamInUse Cname) -> Id ()
-- mkCname (MkPIU _) = pure ()

-- mkCnstr : (1 cnstr : ParamInUse Cnstr) -> List String -> Id ()
-- mkCnstr (MkPIU _) l = pure ()

                        
-- foo3: ()
-- foo3 = with FMT.pure
--        with FMT.(>>=)
--        createObj $ (\cname => \cnstr => 
--          do
--            v1 <- mkCname cname 
--            v <- mkCnstr cnstr []
--            pure v
--          )
         

-- data XMonad : (v : Type) -> Type where
--   MkXMonad : v -> XMonad v

-- abs : {1 x : AbstractFlag} -> XMonad ()
-- abs {x=x} = pure ()
 
--infixl / infixr makes no difference (it doesn't really make sense in
--this context, anyway)
namespace B
  infixr 5 >>=
  
  export
  (>>=) : Nat -> (() -> Nat) -> Nat
  (>>=) x fy = x + (fy ()) * 2

test9 : Nat
test9 = B.do
         5
         6
         7
         
-- test10 : Nat
-- test10 = let (>>=) = B.(>>=)
--          in
--            5 >>= (\_ =>  6) >>= (\_ => 7)

--infixr doesn't make sense because then we'd have to process (\_ => 6) >>= (\_ => 7) and (\_ => 6) isn't a Nat
--therefore we can't have the infixr/infixl affect the do which is always infixl
--just like in a monad "m a -> (a -> m b) -> m b", we can't do (a -> m b) >>= (b -> m c)
 
namespace A
  infixl 5 >>=
  
  export
  (>>=) : Nat -> (() -> Nat) -> Nat
  (>>=) x fy = x + (fy ()) * 2

test5 : Nat
test5 = A.do
         5
         6
         7

--this is how the do processes it. The second arg can be fixed to
--the result should be at the end. ie.
--(>>=) intermediateValue functionProducingFinalResult
test6 : Nat
test6 = let (>>=) = A.(>>=)
        in
          (5 >>= (\_ =>  6 >>= (\_ => 7)))

--same as test6, just in prefix order
test6A : Nat
test6A = let (>>=) = A.(>>=)
         in
           (>>=) 5 (\_ =>  (>>=) 6 (\_ => 7))
           
--this is an alternative way, but not how "do" does it. 
--(>>=) initialValueToDo functionProducingIntermediateResult
test7 : Nat
test7 = let (>>=) = A.(>>=)
        in
          (5 >>= (\_ =>  6)) >>= (\_ => 7)
 
test8 : Nat
test8 = let (>>=) = A.(>>=)
        in
          5 >>= (\_ =>  6) >>= (\_ => 7)

--but i think we can flip it so we get the data we want
namespace C
  data MyTakeTheNat : Nat -> Type where
    MkIt : MyTakeTheNat x

  --now the init value is available to the function so we can get at
  --the prior value when deciding what the result of the function
  --should be, at compile time.
  --(>>=) : (x : Nat) -> ((MyTakeTheNat x) -> Nat) -> Nat

namespace D
  data MySeq : Nat -> Nat -> Type where
    MkMySeq : (x : Nat) -> MySeq x (S x)
    JoinMySeq : MySeq x y -> MySeq y z -> MySeq x z 
    
  -- data MoveSeq : MySeq x (S x) -> MySeq (S x) y -> Type where
  --   MkMoveSeq : MoveSeq (MkMySeq (S x)) (MkMySeq y)

  -- pure : Nat -> MySeq v (S v)
  -- pure v = MkMySeq v

  (>>=) : (sx : MySeq x y) -> (fsy : () -> (MySeq y z)) -> MySeq x z
  (>>=) sx fsy = JoinMySeq sx (fsy ())
  
  fooxxx : Nat
  fooxxx = 42

  test10 : MySeq 1 6
  test10 = 
    D.do 
        MkMySeq 1
        MkMySeq 2
        MkMySeq 3
        MkMySeq 4 
        MkMySeq 5

  test11 : MySeq 1 3
  test11 = let (>>=) = D.(>>=)
           in
             (MkMySeq 1 >>= (\_ =>  MkMySeq 2))
             -- (MkMySeq 0 >>= (\_ =>  MkMySeq 1 >>= (\_ => MyMySeq 2)))
namespace E
  data MySeq : (start: Nat) -> (curr : Nat) -> Type where
    MkMySeq : (start : Nat) -> MySeq start (start + start)
    JoinMySeq : MySeq x y -> MySeq y z -> MySeq x z 
    
  -- data MoveSeq : MySeq x (S x) -> MySeq (S x) y -> Type where
  --   MkMoveSeq : MoveSeq (MkMySeq (S x)) (MkMySeq y)

  -- pure : Nat -> MySeq v (S v)
  -- pure v = MkMySeq v

  public export  
  (>>=) : (MySeq w x -> MySeq x y) -> (fsy : () -> (MySeq w y -> MySeq y z)) -> MySeq w z
  (>>=) fwy ffwz = (\swx => JoinMySeq (fwy swx) (ffwz () (fwy swx)))
  
  -- fooxxx : Nat
  -- fooxxx = 42

  -- test10 : MySeq 1 6
  -- test10 = 
  --   D.do 
  --       MkMySeq 1
  --       MkMySeq 2
  --       MkMySeq 3
  --       MkMySeq 4 
  --       MkMySeq 5

  -- test11 : MySeq 1 3
  -- test11 = let (>>=) = D.(>>=)
  --          in
  --            (MkMySeq 1 >>= (\_ =>  MkMySeq 2))
  --            -- (MkMySeq 0 >>= (\_ =>  MkMySeq 1 >>= (\_ => MyMySeq 2)))
