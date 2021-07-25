

foo : {0 s : Type} -> (s -> Int) -> Bool
foo f = True


footest : Bool
footest = foo (+ 1)

foo2 : {0 s : Type} -> (Int -> s) -> Bool
foo2 f = True


foo2test : Bool
foo2test = foo2 (+ 1)

foo3 : {0 s : Type} -> (Int -> s) -> s
foo3 f = f 5

foo3test : Int
foo3test = foo3 (+ 1)

foo5 : {0 a : Type} -> (Int -> a) -> Bool

foo5test : Bool
foo5test = foo5 (+ 1)
