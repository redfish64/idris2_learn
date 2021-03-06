module LearnLinArray2

import Data.IOArray

-- Linear arrays. General idea: mutable arrays are constructed linearly,
-- using newArray. Once everything is set up, they can be converted to
-- read only arrays with constant time, pure, access, using toIArray.

-- Immutable arrays which can be read in constant time, but not updated
public export
interface Array arr where
  read : (1 _ : arr t) -> Int -> Maybe t
  size : (1 _ : arr t) -> Int

-- Mutable arrays which can be used linearly
public export
interface Array arr => MArray arr where
  newArray : (size : Int) -> (1 _ : (1 _ : arr t) -> a) -> a
  -- Array is unchanged if the index is out of bounds
  write : (1 _ : arr t) -> Int -> t -> arr t

  mread : (1 _ : arr t) -> Int -> Res (Maybe t) (const (arr t))
  msize : (1 _ : arr t) -> Res Int (const (arr t))

export
data IArray : Type -> Type where
     MkIArray : IOArray t -> IArray t

export
data LinArray : Type -> Type where
     MkLinArray : IOArray t -> LinArray t

-- Convert a mutable array to an immutable array

export
toIArray : (1 _ : LinArray t) -> (IArray t -> a) -> a
toIArray (MkLinArray arr) k = k (MkIArray arr)

export
Array LinArray where
  read (MkLinArray a) i = unsafePerformIO (readArray a i)
  size (MkLinArray a) = max a

export
MArray LinArray where
  newArray size k = k (MkLinArray (unsafePerformIO (newArray size)))

  write (MkLinArray a) i el
      = unsafePerformIO (do writeArray a i el
                            pure (MkLinArray a))
  msize (MkLinArray a) = max a # MkLinArray a
  mread (MkLinArray a) i
      = unsafePerformIO (readArray a i) # MkLinArray a

export
Array IArray where
  read (MkIArray a) i = unsafePerformIO (readArray a i)
  size (MkIArray a) = max a

export
copyArray : MArray arr => (newsize : Int) -> (1 _ : arr t) ->
            (arr t)
copyArray newsize a
    = let size # a = msize a in
          LearnLinArray2.newArray newsize $
            (\a' => 
               case copyContent (min (newsize - 1) (size - 1)) a a' of
                 (x # y) => ?xxx_1
            )
  where 
    copyContent : Int -> (1 _ : arr t) -> (1 _ : arr t) -> LPair (arr t) (arr t)
    copyContent pos a a'
        = if pos < 0
             then a # a'
             else let val # a = mread a pos
                      1 a' = case val of
                                  Nothing => a'
                                  Just v => write a' pos v in
                      copyContent (pos - 1) a a'

                      
                                                                  
-- --TIM E. : by pattern matching on it, we are "using" it, and therefore it's no longer linear                      
-- -- - + LearnLinArray2.xxx_ [P]
-- --  `--                 0  t : Type
-- --                         x : IOArray t
-- --                   newsize : Int
-- --      ----------------------------------
-- --       LearnLinArray2.xxx_ : LinArray t
-- growArray : (newsize : Int) -> (1 _ : LinArray t) ->
--             LinArray t
-- growArray newsize (MkLinArray x) =  ?xxx_

-- --TIM E. : here we don't pattern match, so it's still linear
-- -- - + LearnLinArray2.xxx2 [P]
-- --  `--                 0  t : Type
-- --                    1  arr : LinArray t
-- --                   newsize : Int
-- --      ----------------------------------
-- --       LearnLinArray2.xxx2 : LinArray t
-- growArray2 : (newsize : Int) -> (1 _ : LinArray t) ->
--             LinArray t
-- growArray2 newsize arr =  ?xxx2
