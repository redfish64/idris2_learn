

foo : Int
foo = let x@(a,b) : (Int, Int) = (42,43)
      in a
      
