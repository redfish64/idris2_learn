import System
import Data.Morphisms

interface Dog x where
  barkAt : x -> x -> String
  getDog : (isSmallest : Bool) -> x

data Husky = FemaleHusky | MaleHusky

data Chiwhawa = BrownChi | WhiteChi

Dog Husky where
  barkAt FemaleHusky FemaleHusky = "grr"
  barkAt FemaleHusky MaleHusky = "..."
  barkAt MaleHusky FemaleHusky = "wolf"
  barkAt MaleHusky MaleHusky = "bark"
  getDog False = MaleHusky
  getDog True = FemaleHusky

Dog Chiwhawa where
  barkAt BrownChi BrownChi = "bark"
  barkAt BrownChi WhiteChi = "bark bark"
  barkAt WhiteChi BrownChi = "bark bark bark bark bark"
  barkAt WhiteChi WhiteChi = "bark bark bark"
  getDog False = BrownChi
  getDog True = WhiteChi


--here , Dog x adds functions into the data value for barkAt and getDog depending on the
-- data that it finds, either Chiwhawa or Husky
data Foo : Type where
  Foo_ : Dog x => (x -> x -> String) -> Foo

-- co: won't work
-- runit : Foo -> Bool
-- runit (Foo_ f) = f FemaleHusky MaleHusky == "Foo"

runit : Foo -> String
runit (Foo_ f) = f (getDog False) (getDog True)

test : IO ()
test =
  let myfoo : Foo = Foo_ {x=Husky} barkAt
      myfoo2 : Foo = Foo_ {x=Chiwhawa} barkAt
  in 
    do
      putStrLn $ show $ runit myfoo
      putStrLn $ show $ runit myfoo2

-- co: won't work
-- runit2 : String
-- runit2 = barkAt (getDog False) (getDog True)
