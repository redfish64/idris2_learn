import System
import Data.Morphisms

public export
data Foo : Type where
  Foo_ : Eq n => Num n => (n -> n -> n) -> Foo

public export
data Fee = One | Zero

public export
Eq Fee where
  One == One = True
  Zero == Zero = True
  _ == _ = False

public export
Num Fee where
  One + One = One
  One + Zero = One
  Zero + One = One
  Zero + Zero = Zero

  fromInteger 0 = 0
  fromInteger _ = One
  
  One * One = One
  One * Zero = Zero
  Zero * _ = Zero
  
export
runit : Foo -> Bool
runit (Foo_ f) = f 1 1 == 1 --how does it choose what 1 should be? Is it Int, Integer, or Fee? How does it know the answer if for all n?


test : IO ()
test =
  let myfoo : Foo = Foo_ (+)
  in
    putStrLn $ show $ runit myfoo
