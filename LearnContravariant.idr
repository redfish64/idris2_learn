import Data.Contravariant
import Control.Lens.Const

--taken from Data.Morphisms
public export
record Op y x where
  constructor MkOp
  applyOp : x -> y
  
public export
Contravariant (Op j) where
  contramap f (MkOp g) = MkOp (g . f)
  v >$ (MkOp f) = MkOp \_ => f v

myOp : Op Char String
myOp = MkOp (the (String -> Char) (const 'x'))

test : Op Char Char
test = contramap (the (Char -> String) (\v => show v)) myOp

test2 : Op Char Int
test2 = contramap (the (Int -> String) (\v => "xxx")) myOp
