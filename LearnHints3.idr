
data Foo : (v : Type -> Type) -> (x : Type) -> Type where
  [noHints]
  Fee : {v : Type -> Type} -> (doit : v x -> Int) -> Foo v x
  Bee : {x : Type} -> (doit : v x -> String) -> Foo v x

%hint
fooMaybe : {x : Type} -> Foo Maybe x
fooMaybe = Bee (\vx => maybe "Dog" (const "Cat") vx)

%hint
fooList : {x : Type} -> Foo List x
fooList = Fee $ cast . length

bar : Foo Maybe Int => Maybe Int -> String
bar @{Fee doit} x = show $ doit x
bar @{Bee doit} x = doit x

bar2 : Foo Maybe Int => Maybe Int -> String
bar2 @{i} x = case i of
                (Fee doit) => ?zzz_1
                (Bee doit) => ?zzz_

