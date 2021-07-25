module LearnVect

import Data.Vect

Size : Nat
Size = 10000

Times : Nat
Times = 10000

countVect : (n : Nat) -> Vect m Int -> Vect (n + m) Int
countVect 0 xs = xs
countVect (S k) xs = believe_me $ countVect k $ cast k :: xs

vect1 : Vect Size Int
vect1 = countVect Size []

loop : Nat -> (Nat -> Int -> Int) -> Int -> Int
loop Z f y = f Z y
loop (S v) f y = loop v f $ f v y

-- loop : Fin v -> (Fin v -> Int -> Int) -> Int -> Int
-- loop FZ f y = f FZ y
-- loop (FS x) f y = loop (weaken x) f (f (FS x) y)

natToWrappingFin : Nat -> Fin Size
natToWrappingFin k = maybe FZ id (natToFin (integerToNat $ cast k `mod` cast Size) Size)


main : IO ()
main = putStrLn $ show $ loop Times 
         (\ind,acc => index (natToWrappingFin (ind * 39)) vect1 + acc) 0
