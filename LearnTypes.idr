--import Builtin

--we need to include an argument or it will be thought of as a variable in the next statement
-- "x : foo ()"
foo : () -> Type                     
foo () = Int

x : foo ()
x = the Int 5

x1 : Int
x1 = the (foo ()) 5

Foo : Type
Foo = Int

x2 : Foo
x2 = the Int 5

p1: Nat -> Type
p1 n = (n=2)

testReplace: (x=y) -> (p1 y) -> (p1 x)
testReplace a b = rewrite__impl p1 a b
--testReplace a b = replace a b
--testReplace a b = rewrite (sym a) in b
