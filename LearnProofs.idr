
data NatHolder : Nat -> Type where
  MkNH : (n : Nat) -> NatHolder n


plusnZ : (n : Nat) -> n + 0 = n
plusnZ 0 = Refl
plusnZ (S k) = 
   ---rewrite plusnZ k in Refl
   let prf : (S k = S k)
       prf = Refl
       prf2 : ((S k) + 0 = (S k))
       prf2 = rewrite plusnZ k in Refl
   in prf2
 
plusnSm : (n, m : Nat) -> n + (S m) = S (n + m)
plusnSm Z m = Refl
plusnSm (S k) m = rewrite plusnSm k m in Refl

plusCommutes : (n, m : Nat) -> n + m = m + n
plusCommutes Z m = sym (plusnZ m)
plusCommutes (S k) m = rewrite plusCommutes k m in sym (plusnSm m k)

--this here is really bizarre in that we can place plusCommutes n m into replace directly, but can't
--put it into a variable

 
foo2 : NatHolder (n + m) -> NatHolder (m + n)
foo2 {n} {m} nh = 
  replace {p=NatHolder} (plusCommutes n m) nh

-- co: doesn't work
-- foo : NatHolder (n + m) -> NatHolder (m + n)
-- foo {n} {m} nh = 
--   let pc : (n + m = m + n)
--       pc = (plusCommutes n m) 
--   in replace {p=NatHolder} pc nh
 
-- co: also doesn't work
-- foo3 : NatHolder (n + m) -> NatHolder (m + n)
-- foo3 {n} {m} nh = 
--   let pc = (plusCommutes n m) 
--   in replace {p=NatHolder} pc nh
 
-- co: also doesn't work
-- foo : NatHolder (n + m) -> NatHolder (m + n)
-- foo {n} {m} nh = 
--   let pc : (n + m = m + n)
--          = (plusCommutes n m) 
--   in replace {p=NatHolder} pc nh
 

-- co: also doesn't work
-- foo : NatHolder (n + m) -> NatHolder (m + n)
-- foo {n} {m} nh = 
--   let pc : (0 pc : n + m = m + n)
--       pc = (plusCommutes n m) 
--   in replace {p=NatHolder} pc nh

