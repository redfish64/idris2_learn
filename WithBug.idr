foo : Int -> Int
foo x with (x + 1)
  foo x | y = y + x

foo2 : Int
foo2 = foo 5

namespace A
  --typechecks by virtue of being at the last thing defined in the namespace identation block
  export
  foo3 : Int -> Int
  foo3 x with (x + 1)
  foo3 x | y = y + x
 
--and still works as expected 
foo4 : Int
foo4 = A.foo3 2

--same as foo3
foo5 : Int -> Int
foo5 x with (x + 1)
foo5 x | y = y + x

--fails to typecheck only due to the lack of indenting of the last foo5 line
foo6 : Int
foo6 = 52

