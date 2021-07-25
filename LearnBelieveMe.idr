import Data.Vect


try1 : {n : Nat} -> {m : Nat} -> Vect (n + m) x -> Vect (m + n) x
try1 xs = believe_me xs


foo : IO ()
foo = putStrLn $ show $ try1 {n=2} {m=3} (the (Vect 5 Int) [1,2,3,4,5])
