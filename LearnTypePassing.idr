


interface Foo (0 x : Type) where
  bar : x -> String

interface Foo2 (0 x : Type) where
  ibar : x -> Int

data Bla : Type where
  Blab : {0 xt : Type} -> Foo xt => xt -> Bla
  Blee : {0 xt : Type} -> Foo2 xt => xt -> Bla
 
--I don't understand how this works... if at runtime we don't have access to xt
-- how do we know which bar to run? It seems to be know the xt at runtime anyway
showTheBlas : List Bla -> List String
showTheBlas [] = []
showTheBlas ((Blab {xt=xt} x) :: xs) = bar x :: showTheBlas xs
showTheBlas ((Blee x) :: xs) = show (ibar x) :: showTheBlas xs

data Pig = Pog | Pleg
data Cow = Cork | Creek

Foo Pig where
  bar Pog = "Pog"
  bar Pleg = "Pleg"
  
Foo String where
  bar s = s

Foo2 Cow where
  ibar Cork = 1
  ibar Creek = 2


blas : List Bla
blas = [Blab Pog, Blee Cork, Blab Pleg, Blee Creek]

data ShowW : Type where
  sw : Show s => s -> ShowW

namespace TypMys 
  interface TypeMystery x where
    tmys : String
    tmys2 : x -> String

  data TypeMysteryW : Type where
    TMW : TypeMystery s => TypeMysteryW

  showTheMysteries : List TypeMysteryW -> String
  showTheMysteries [] = "."
  showTheMysteries (x@(TMW {s=xt}) :: xs) = "" --?xxx
  
  
