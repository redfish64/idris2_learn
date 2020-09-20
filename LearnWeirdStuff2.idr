module LearnWeirdStuff2

import LearnWeirdStuff

-- test5 : Int
-- test5 = foo 5
 
-- test6 : String
-- test6 = foo "fee"
 
data Bla : Type where
  MkBla : Int -> Bla

-- foo : _
-- foo =
--   let x = MkBla 1
--       y = MkBla 1
--   in

testfoo6 : Int
testfoo6 = foo6 6

testfoo6' : Int
testfoo6' = foo6 7 8
