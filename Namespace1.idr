module Namespace1

namespace X1 
  export 
  foo : Int -> Int
  foo = (+ 1)
  
namespace X2 
  export 
  foo : String -> String
  foo = (++ "bar")
 
test1 : Int
test1 = foo 5
 
test2 : String
test2 = foo "foo"
 
 
