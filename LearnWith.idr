module LearnWith

import Data.Vect
import Data.Nat as N

--from Idris, a General Purpose Dependently Typed Programming Language: Design and Implementation

filter : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter p [] = ( _ ** [] )
filter p (x :: xs) with (LearnWith.filter p xs)
  filter p (x :: xs) | ( _ ** xs' ) = if (p x) then ( _ ** x :: xs' ) else ( _ ** xs' )
  
data Parity : Nat -> Type where
  Even : {n : Nat} -> Parity (n + n)
  Odd : {n : Nat} -> Parity (S (n + n))

parity : (n:Nat) -> Parity n
-- parity Z = Even {n = Z}
-- parity (S Z) = Odd {n = Z}
-- parity (S (S k)) with (parity k)
--   parity (S (S (j + j))) | Even
--       = rewrite N.plusSuccRightSucc j j in Even {n = S j}
--   parity (S (S (S (j + j)))) | Odd
--       = rewrite N.plusSuccRightSucc j j in Odd {n = S j}
      
natToBin : Nat -> List Bool
natToBin Z = Nil
natToBin k with (parity k)
  natToBin (j + j) | Even = False :: natToBin j
  natToBin (S (j + j)) | Odd = True :: natToBin j
  
  -- natToBin ?x | even = ?y
  
data Namespace : Type where
  MkNS : List String -> Namespace
 
public export
data Name : Type where
     NS : Namespace -> Name -> Name -- in a namespace
     UN : String -> Name -- user defined name
     MN : String -> Int -> Name -- machine generated name
     PV : Name -> Int -> Name -- pattern variable name; int is the resolved function id
     DN : String -> Name -> Name -- a name and how to display it
     RF : String -> Name  -- record field name
     Nested : (Int, Int) -> Name -> Name -- nested function name
     CaseBlock : String -> Int -> Name -- case block nested in (resolved) name
     WithBlock : String -> Int -> Name -- with block nested in (resolved) name
     Resolved : Int -> Name -- resolved, index into context
     
data Token
  -- Literals
  = CharLit String
  | DoubleLit Double
  | IntegerLit Integer
  | StringLit String
  -- Identifiers
  | HoleIdent String
  | Ident String
  | DotSepIdent Namespace String -- ident.ident
  | DotIdent String               -- .ident
  | Symbol String
  -- Comments
  | Comment String
  | DocComment String
  -- Special
  | CGDirective String
  | EndInput
  | Keyword String
  | Pragma String
  | Unrecognised String
     
public export
record WithBounds ty where
  constructor MkBounded
  val : ty
  isIrrelevant : Bool
  startLine : Int
  startCol : Int
  endLine : Int
  endCol : Int
  
lex : String -> Either () (List (WithBounds Token))
lex x = Right [MkBounded (DotSepIdent (MkNS ["bar"]) "foo") False 0 0 0 0]
     
convertStringToName : String -> Name
convertStringToName v with (lex v)
  convertStringToName v | Right [b] with (val b)
    convertStringToName v | Right [b] | DotSepIdent ns n =  NS ns (UN n)
    convertStringToName v | Right [b] | _ =  (UN v)
  convertStringToName v | _ = (UN v)

foo : Int -> Int -> Bool
foo n m with (n + 1)
  foo _ m | 2 with (m + 1)
    foo _ _ | 2 | 3 = True
    foo _ _ | 2 | _ = False
  foo _ _ | _ = False
 

-- doesn't work 
-- filter' : (a -> Bool) -> Vect n a -> (p ** Vect p a)
-- filter' p [] = ( _ ** [] )
-- filter' p (x :: xs) with (filter' p xs)
--   | ( _ ** xs' ) = if (p x) then ( _ ** x :: xs' ) else ( _ ** xs' )
  
