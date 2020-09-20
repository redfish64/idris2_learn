--from Idris, a General Purpose Dependently Typed Programming Language: Design and Implementation

data Parity : Nat -> Type where
  even : Parity (n + n)
  odd : Parity (S (n + n))
  
-- parity : (n:Nat) -> Parity n

-- co: doesn't work
-- natToBin : Nat -> List Bool
-- natToBin Z = Nil
-- natToBin k with (parity k)
--   natToBin (j + j) | even = False :: natToBin j
--   natToBin (S (j + j)) | odd = True :: natToBin j
  
  --natToBin ?x | even = ?y
