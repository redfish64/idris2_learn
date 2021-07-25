
interface Bar x where
  bar : (1 _ : x) -> Int

foo : Bar x => x -> Int
foo x = 
  let y = bar x
  in y

-- foo1 : (1 x : Type) -> Bar x => Int
-- foo1 x = 
--   let y = foo x --won't work because foo isn't linear
--   in ?xxx

-- test1 : (1 x : Int) -> (1 y : Int) -> Int
-- test1 x y = 
--   if ((\x1 => True) x) == True then y else y
