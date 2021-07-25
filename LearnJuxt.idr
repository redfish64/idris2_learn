
import Data.HVect
import Data.Vect

From : Type -> Vect n Type -> Vect n Type
From a [] = []
From a (x :: xs) = (a -> x) :: From a xs

juxt : {k : Nat} -> {ts : Vect k Type} -> HVect (From a ts) -> a -> HVect ts
juxt {k = Z} {ts = []} [] x = []
juxt {k = (S len)} {ts = (y :: xs)} (f :: fs) x = f x :: juxt {k = len} {ts = xs} fs x

fs : HVect (From Int [Int, Char, Bool])
fs = [(+ 1), chr, (> 10)]

result : HVect [Int, Char, Bool]
result = juxt fs 12

-- Less hints for unification

fs' : HVect [Int -> Int, Int -> Char, Int -> Bool]
fs' = [(+ 1), chr, (> 10)]

result' : HVect [Int, Char, Bool]
result' = juxt fs' 12

result'' : HVect [Int, Char, Bool]
result'' = juxt [(+ 1), chr, (> 10)] 12

curried : Int -> HVect [Int, Char, Bool]
curried = juxt fs'

