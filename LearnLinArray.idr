import Data.Linear.Array

-- lFoldr : (elem -> acc -> acc) -> acc -> LinArray elem -> acc
-- lFoldr f a l = ?xx

Foldable IArray where
  foldr f a l = 
    let s = size l
    in doit a (s - 1)
   where 
     doit : acc -> Int -> acc
     doit acc p = if (p < 0) then acc else 
        let mv = read l p
        in case mv of 
              Nothing => doit acc (p-1)
              Just v => doit (f v acc) (p-1)
       

writeIndexes : (1 _ : LinArray Int) -> LinArray Int
writeIndexes arr = 
  let (asize # arr1) : Res Int (const (LinArray Int)) = msize arr
  -- let asize = size arr 
  --     arr1 = arr -- co: doesn't work, arr becomes "0". I'm guessing the idea here is that you aren't allowed to
  -- update the array after reading it (including the size). That's why it takes it away
  -- from you after doing a size (as long as the arg passed is actually linear, IArray isn't so it can be read
  -- multiple times)
  in writeIndexes1 (asize - 1) arr1
  where
   writeIndexes1 : Int -> (1 _ : LinArray Int) -> LinArray Int
   writeIndexes1 rem arr = case rem < 0 of 
     True => arr
     False => writeIndexes1 (rem - 1) (write arr rem (rem+1))

test : Int
test = 
  newArray 5 doit
 where
   doit : (1 _ : LinArray Int) -> Int
   doit arr = 
     let arr2 = writeIndexes arr
     in 
       --toIArray arr2 $ (\arr3 => maybe (-9999) id (read arr3 0))
       toIArray arr2 $ foldr (+) 0
 
runtest : IO ()
runtest = putStrLn $ show test
