

record Foo where
  constructor MkFoo
  bar : Int
  bee : Char

x : Foo
x = MkFoo 42 'x'

y : Foo
y = record { bar = 43 } x

z : Int
z = x.bar

record Foo2 where
  constructor MkFoo2
  bar : Int
  bee : Char
  { bish : String }

test : Foo2 -> Foo2
test f = record { bar = 5, bish=f.bish } f --note, need bish due to https://github.com/idris-lang/Idris2/issues/429


