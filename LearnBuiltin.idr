module LearnBuiltin

import Data.Fin

-- both Nat1 and Nat2 are copies of Nat.  Nat2 gets the special builtin treatment for its nat

namespace N1
  ||| Natural numbers: unbounded, unsigned integers which can be pattern matched.
  public export
  data Nat1 =
    ||| Zero.
      Z
    | ||| Successor.
    S Nat1
  
  public export
  integerToNat1 : Integer -> Nat1
  integerToNat1 0 = Z -- Force evaluation and hencing caching of x at compile time
  integerToNat1 x
    = if intToBool (prim__lte_Integer x 0)
         then Z
         else S (assert_total (integerToNat1 (prim__sub_Integer x 1)))

  public export
  natToInteger1 : Nat1 -> Integer
  natToInteger1 Z = 0
  natToInteger1 (S k) = 1 + natToInteger1 k
                           -- integer (+) may be non-linear in second
                           -- argument

  -- Define separately so we can spot the name when optimising Nats
  ||| Add two natural numbers.
  ||| @ x the number to case-split on
  ||| @ y the other numberpublic export
  public export
  plus : (x : Nat1) -> (y : Nat1) -> Nat1
  plus Z y = y
  plus (S k) y = S (plus k y)

  ||| Subtract natural numbers.  If the second number is larger than the first,
  ||| return 0.
  public export
  minus : (left : Nat1) -> Nat1 -> Nat1
  minus Z        right     = Z
  minus left     Z         = left
  minus (S left) (S right) = minus left right

  ||| Multiply natural numbers.
  public export
  mult : (x : Nat1) -> Nat1 -> Nat1
  mult Z y = Z
  mult (S k) y = plus y (mult k y)

public export
Num Nat1 where
  (+) = plus
  (*) = mult

  fromInteger x = integerToNat1 x

public export
Eq Nat1 where
  Z == Z = True
  S j == S k = j == k
  _ == _ = False

public export
Ord Nat1 where
  compare Z Z = EQ
  compare Z (S k) = LT
  compare (S k) Z = GT
  compare (S j) (S k) = compare j k
  
public export
Show Nat1 where
  show x = show $ natToInteger1 x




namespace N2
  ||| Natural numbers: unbounded, unsigned integers which can be pattern matched.
  public export
  data Nat2 =
    ||| Zero.
      Z
    | ||| Successor.
    S Nat2
  
  -- This is used in the compiler as an efficient substitute for integerToNat.
  prim__integerToNat : Integer -> Nat2
  prim__integerToNat i
    = if intToBool (prim__lte_Integer 0 i)
        then believe_me i
        else Z

  public export
  integerToNat2 : Integer -> Nat2
  integerToNat2 0 = Z -- Force evaluation and hencing caching of x at compile time
  integerToNat2 x
    = if intToBool (prim__lte_Integer x 0)
         then Z
         else S (assert_total (integerToNat2 (prim__sub_Integer x 1)))

  public export
  natToInteger2 : Nat2 -> Integer
  natToInteger2 Z = 0
  natToInteger2 (S k) = 1 + natToInteger2 k
                           -- integer (+) may be non-linear in second
                           -- argument

  -- Define separately so we can spot the name when optimising Nats
  ||| Add two natural numbers.
  ||| @ x the number to case-split on
  ||| @ y the other numberpublic export
  public export
  plus : (x : Nat2) -> (y : Nat2) -> Nat2
  plus Z y = y
  plus (S k) y = S (plus k y)

  ||| Subtract natural numbers.  If the second number is larger than the first,
  ||| return 0.
  public export
  minus : (left : Nat2) -> Nat2 -> Nat2
  minus Z        right     = Z
  minus left     Z         = left
  minus (S left) (S right) = minus left right

  ||| Multiply natural numbers.
  public export
  mult : (x : Nat2) -> Nat2 -> Nat2
  mult Z y = Z
  mult (S k) y = plus y (mult k y)

public export
Num Nat2 where
  (+) = plus
  (*) = mult

  fromInteger x = integerToNat2 x

public export
Eq Nat2 where
  Z == Z = True
  S j == S k = j == k
  _ == _ = False

public export
Ord Nat2 where
  compare Z Z = EQ
  compare Z (S k) = LT
  compare (S k) Z = GT
  compare (S j) (S k) = compare j k
  
public export
Show Nat2 where
  show x = show $ natToInteger2 x


foo : Nat
foo = 39999999

testFoo : IO ()
testFoo = putStrLn (show foo)

foo1 : Nat1
foo1 = 39999999

testFoo1 : IO ()
testFoo1 = putStrLn (show foo1)

--foo2 uses the builtins so is faster
--but Nat is just as fast
%builtin Natural Nat2
%builtin NaturalToInteger natToInteger2
%builtin IntegerToNatural integerToNat2

foo2 : Nat2
foo2 = 39999999

testFoo2 : IO ()
testFoo2 = putStrLn (show foo2)
