

interface Bar a where
  toChar : a -> Char

Bar Int where
  toChar 0 = 'a'
  toChar _ = 'b'
  

foo : (1 _ : Int) -> (Char -> a) -> a
foo x k = k (toChar x)


-- test1 : (1 x : Int) -> (1 y : Int) -> Int
-- test1 x y = 
--   if ((\x1 => True) x) == True then y else y
