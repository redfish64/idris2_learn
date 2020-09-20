foo28 : (0 x : Bool) -> Bool
foo28 _ = True

linearOr : (1 x : Bool) -> (1 y : Bool) -> Bool
linearOr True True = True
linearOr True False = True
linearOr False True = True
linearOr False False = False

foo27 : (1 x : Bool) -> Bool
foo27 x = linearOr (foo28 x) x
