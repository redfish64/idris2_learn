import Data.Nat

foo : (n : Nat) -> LTE n 4 -> Nat
foo 1 (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))) impossible
foo 2 (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))) impossible
foo 3 (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))) impossible
foo 4 (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))) impossible
foo (S (S (S (S (S _))))) (LTESucc (LTESucc (LTESucc (LTESucc (LTESucc _))))) impossible

--https://github.com/idris-lang/Idris2/issues/484
