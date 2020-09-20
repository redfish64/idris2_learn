module Main

import Data.Vect

main : IO ()
main = putStrLn "Hello world"

invert : Bool -> Bool
invert True = ?invert_rhs_1
invert False = ?invert_rhs_

vadd : Num a => Vect n a -> Vect n a -> Vect n a
vadd [] [] = []
vadd (x :: xs) (y :: ys) = x+y :: vadd xs ys



