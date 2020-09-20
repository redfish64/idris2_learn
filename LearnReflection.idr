import Language.Reflection

foo : TTImp
foo = `(Z)

foo2 : TTImp
foo2 = `(Int -> Int)

foo3 : TTImp
foo3 = `((1 x : Int) -> Int)

foo4 : TTImp
foo4 = `((0 x : Int) -> Int)

--"IPrimVal"
foo5: TTImp
foo5 = `("Foo")

--special "IType"
foo6: TTImp
foo6 = `(Type)

--this is "IVar", just like Z
foo7: TTImp
foo7 = `(TTImp)

oneOrZero : Maybe () -> Int
oneOrZero (Just ()) = 1
oneOrZero Nothing = 0

foo8 : TTImp
foo8 = `(oneOrZero)

names : List Name
names = [ `{{ namesfdafds }}, `{{ Prelude.(+) }} ]
