
-- example of a interface that can be implemented two ways, Fee and Bee
data Foo : Type -> Type where
  [noHints]
  Fee : (doit : v -> Int) -> Foo v
  Bee : (doit : v -> String) -> Foo v

%hint
fooBool : Foo Bool
fooBool = Bee (\x => if x then "Dog" else "Cat")

%hint
fooInteger : Foo Integer
fooInteger = Fee cast

%hint
fooShowable : Show x => Foo x
fooShowable = Bee (\x => "def: "++show x)

--Here we use interface
test : Foo x => x -> String
test @{Bee f} x = f x
test @{Fee f} x = show $ f x

test1 : String
test1 = test True

test2 : String
test2 = test 6

test3 : String
test3 = test 'c'
