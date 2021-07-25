data Foo : Type where [noHints]
  A : Foo
  B : Foo

findA : {auto foo : Foo} -> String
findA {foo = A} = "Found an A"
findA {foo = _} = "Failed to find an A"

Baz : String -> Type
Baz s = s = "Found an A"


baz : (s : String ** Baz s)
baz = let %hint arg : Foo
          arg = A
      in (findA ** Refl)


-- --var in first part of dpair is available in second, even as a type
foo : (vt ** vt)
foo = (String ** "foo")

foo1 : (vt : Type **  vt)
foo1 = (String ** "foo")

foo2 : DPair Type Prelude.id
foo2 = (String ** "foo")

foo3 : DPair Type (\v => v)
foo3 = (String ** "foo")

foo4 : (vt : Type ** vt2 : Type ** (vt,vt2))
foo4 = (String ** Int ** ("foo",42))

foo4' : (vt ** vt2 ** (vt,vt2))
foo4' = (String ** Int ** ("foo",42))

mappen : (d : DPair Type (\v => v)) -> fst d
mappen (MkDPair vt v)= v
