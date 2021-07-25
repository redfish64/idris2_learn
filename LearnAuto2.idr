import Data.So


namespace A
  public export
  foo1 : (x : Bool) -> {auto prf : So x} -> Int
  foo1 _ = 42

namespace B 
  public export
  foo2 : Bool
  foo2 = True
  
  export
  foo2' : Bool
  foo2' = True
  
  public export
  foo2_1 : Bool
  foo2_1 = foo2'
  
  pfoo : Bool
  pfoo = True
  
  public export
  foo2_2 : Bool
  foo2_2 = pfoo
  
  
namespace C
  foo3 : Int
  foo3 = foo1 foo2 -- works
  
  foo3' : Int
  foo3' = foo1 foo2' -- doesn't work, foo2' has to be public export
  
  --%logging 3
  
  foo4 : Int
  foo4 = foo1 foo2_1 -- also doesn't work
