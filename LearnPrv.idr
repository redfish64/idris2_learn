module LearnPrv
import Data.So

data PrvOp : Type where
  GT : x -> x -> PrvOp
  EQ : x -> x -> PrvOp

data Prv : PrvOp -> Type where
  GT_ : Ord t => {a,b : t} -> (So $ a > b) -> Prv (GT a b)
  GTofGT : {0 a,b,c : t} -> Prv (GT a b) -> Prv (GT b c) -> Prv (GT a c)
  EQ_ : Eq t => {a,b : t} -> (So $ a == b) -> Prv (EQ a b)
  EQofEQ : {0 a,b,c : t} -> Prv (EQ a b) -> Prv (EQ b c) -> Prv (EQ a c)


fooey : (a : Integer) -> (b : Integer) -> {0 prf : Prv $ GT a b} -> Integer
fooey _ _  = 5

frip : (a : Integer) -> (b : Integer) -> (c : Integer) -> {0 prf : Prv $ GT a b} -> {0 prf2 : Prv $ GT b c} ->
                                              {auto 0 prf3 : Prv $ GT a c} -> Integer
frip _ _ _ = 7                                              

frilp : (a : Integer) -> (b : Integer) -> (c : Integer) -> (d : Integer) 
        -> {0 prf : Prv $ GT a b} 
        -> {0 prf2 : Prv $ GT b c} 
        -> {0 prf3 : Prv $ GT c d}
        -> {auto 0 prf4 : Prv $ GT a d} -> Integer
frilp _ _ _ _ = 4

test1 : Integer
test1 = frip (the Integer 3) 2 1 {prf=GT_ Oh} {prf2=GT_ Oh} {prf3=GT_ Oh} --prf3 must be specified here, even
                                                                          --though it's obvious and has an
                                                                          --auto tag

t3 : forall a. forall b. forall c. Prv (GT a b) -> Prv (GT b c) -> Prv (GT a c)
t3 p1 p2 = GTofGT p1 p2

t3' : Prv (GT a b) -> Prv (GT b c) -> Prv (GT a c)
t3' p1 p2 = GTofGT p1 p2

test2 : Integer
test2 = frip (the Integer 3) 2 1 {prf=p32} {prf2=p21} {prf3=GTofGT p32 p21}
  where
    p32 : Prv (GT 3 2)
    p32 = GT_ Oh
    p21 : Prv (GT 2 1)
    p21 = GT_ Oh




-- namespace T2
--   data PrvOp : Type where
--     GT : x -> x -> PrvOp
--     EQ : x -> x -> PrvOp

--   data Prv : (t : Type) -> PrvOp -> Type where
--     GT_ : Ord t => {a,b : t} -> (So $ a > b) -> Prv t (GT a b)
--     GTofGT : Prv t (GT a b) -> Prv t (GT b c) -> Prv t (GT a c)
--     EQ_ : Eq t => {a,b : t} -> (So $ a == b) -> Prv t (EQ a b)
--     EQofEQ : Prv t (EQ a b) -> Prv t (EQ b c) -> Prv t (EQ a c)


--   fooey : (a : Integer) -> (b : Integer) -> {0 prf : Prv Integer $ GT a b} -> Integer
--   fooey _ _  = 5

--   frip : (a : Integer) -> (b : Integer) -> (c : Integer) -> {0 prf : Prv Integer $ GT a b} -> {0 prf2 : Prv Integer $ GT a b} ->
--                                                 {auto 0 prf3 : Prv Integer $ GT a c} -> Integer
--   frip _ _ _ = 7                                              

--   frilp : (a : Integer) -> (b : Integer) -> (c : Integer) -> (d : Integer) 
--           -> {0 prf : Prv Integer $ GT a b} 
--           -> {0 prf2 : Prv Integer $ GT b c} 
--           -> {0 prf3 : Prv Integer $ GT c d}
--           -> {auto 0 prf4 : Prv Integer $ GT a d} -> Integer
--   frilp _ _ _ _ = 4

--   test1 : Integer
--   test1 = frip (the Integer 3) 2 1 {prf=GT_ Oh} {prf2=GT_ Oh} {prf3=GT_ Oh} --prf3 still must be specified here, even
--                                                                             --though it's obvious and has an
--                                                                             --auto tag, and we know the type is integer

--   -- test2 : Integer
--   -- test2 = frip (the Integer 3) 2 1 {prf=GT_ Oh} {prf2=GT_ Oh} {prf3=doit}
--   --   where
--   --     doit : Prv Integer $ GT 3 1
--   --     doit = ?doit_rhs
  
-- namespace T3
--   data PrvOp : Type where
--     GT : x -> x -> PrvOp
--     EQ : x -> x -> PrvOp

--   data Prv : (Ord t) => (Num t) => (t : Type) -> PrvOp -> Type where
--     GT_ : Ord t => Num t => {a,b : t} -> (So $ a > b) -> Prv t (GT a b)
--     GTofGT : Prv t (GT a b) -> Prv t (GT b c) -> Prv t (GT a c)
--     EQ_ : Eq t => {a,b : t} -> (So $ a == b) -> Prv t (EQ a b)
--     EQofEQ : Prv t (EQ a b) -> Prv t (EQ b c) -> Prv t (EQ a c)


--   fooey : (a : Integer) -> (b : Integer) -> {0 prf : Prv Integer $ GT a b} -> Integer
--   fooey _ _  = 5

--   frip : (a : Integer) -> (b : Integer) -> (c : Integer) -> {0 prf : Prv Integer $ GT a b} -> {0 prf2 : Prv Integer $ GT a b} ->
--                                                 {auto 0 prf3 : Prv Integer $ GT a c} -> Integer
--   frip _ _ _ = 7                                              

--   frilp : (a : Integer) -> (b : Integer) -> (c : Integer) -> (d : Integer) 
--           -> {0 prf : Prv Integer $ GT a b} 
--           -> {0 prf2 : Prv Integer $ GT b c} 
--           -> {0 prf3 : Prv Integer $ GT c d}
--           -> {auto 0 prf4 : Prv Integer $ GT a d} -> Integer
--   frilp _ _ _ _ = 4

--   test1 : Integer
--   test1 = frip (the Integer 3) 2 1 {prf=GT_ Oh} {prf2=GT_ Oh} {prf3=GT_ Oh} --prf3 even still must be specified here, even
--                                                                             --though it's obvious and has an
--                                                                             --auto tag, and we know the type is integer and

--   -- test2 : Integer
--   -- test2 = frip (the Integer 3) 2 1 {prf=GT_ Oh} {prf2=GT_ Oh} {prf3=doit}
--   --   where
--   --     doit : Prv Integer $ GT 3 1
--   --     doit = ?doit_rhs
