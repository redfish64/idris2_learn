foo32 : (1 x : Int) -> Int
foo32 x = case x of 
            0 => 0
            _ => 1
            
foo32' : (1 x : Int) -> Int
foo32' 0 = 0
foo32' _ = 1

