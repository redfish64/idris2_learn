import Data.SortedSet
import Language.Elab.Deriving.Eq
import Language.Elab.Deriving.Ord

%language ElabReflection

data MyNamespace : Type where
  MkNS : List String -> MyNamespace

Eq MyNamespace where
  (==) (MkNS xs) (MkNS ys) = xs == ys
 
Ord MyNamespace where
  compare (MkNS xs) (MkNS ys) = compare xs ys
    
public export
data MyName : Type where
     NS : MyNamespace -> MyName -> MyName -- in a namespace
     UN : String -> MyName -- user defined name
     MN : String -> Int -> MyName -- machine generated name
     PV : MyName -> Int -> MyName -- pattern variable name; int is the resolved function id
     DN : String -> MyName -> MyName -- a name and how to display it
     RF : String -> MyName  -- record field name
     Nested : (Int, Int) -> MyName -> MyName -- nested function name
     CaseBlock : String -> Int -> MyName -- case block nested in (resolved) name
     WithBlock : String -> Int -> MyName -- with block nested in (resolved) name
     Resolved : Int -> MyName -- resolved, index into context

Eq MyName where
  (==) (NS x1 y1) (NS x2 y2) =  x1 == x2 && y1 == y2
  (==) (NS x1 y1) _ =  False
  (==) (UN x) (UN y) = x == y
  (==) (UN x) _ = False
  (==) (MN x y) (MN z w) = x == z && y == w
  (==) (MN x y) _ = False
  (==) (PV x y) (PV w z) = x == w && y == z
  (==) (PV x y) _ = False
  (==) (DN x y) (DN w z) = x == w && y == z
  (==) (DN x y) _ = False
  (==) (RF x) (RF y) = x == y
  (==) (RF x) _ = False
  (==) (Nested x y) (Nested w z) = x == w && y == z
  (==) (Nested x y) _ = False
  (==) (CaseBlock x y) (CaseBlock w z) = x == w && y == z
  (==) (CaseBlock x y) _ = False
  (==) (WithBlock x y) (WithBlock w z) = x == w && y == z
  (==) (WithBlock x y) _ = False
  (==) (Resolved x) (Resolved y) = x == y
  (==) (Resolved x) _ = False

%runElab deriveOrd Private `{{MyName}}

-- Ord MyName where
--   compare (NS x y) (NS w z) = ?xx_1
--   compare (UN x) n2 = ?xx_2
--   compare (MN x y) n2 = ?xx_3
--   compare (PV x y) n2 = ?xx_4
--   compare (DN x y) n2 = ?xx_5
--   compare (RF x) n2 = ?xx_6
--   compare (Nested x y) n2 = ?xx_7
--   compare (CaseBlock x y) n2 = ?xx_8
--   compare (WithBlock x y) n2 = ?xx_9
--   compare (Resolved x) n2 = ?xx_aaa1
               
-- foo : SortedSet MyName
-- foo = empty
