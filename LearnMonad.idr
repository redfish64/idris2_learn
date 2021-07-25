

foo : IO Int
foo = do
        x <- map (the Int) $ pure 5
        pure x
        
