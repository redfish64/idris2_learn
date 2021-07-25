import Data.IORef

data Foo : Type where
  MkFoo : Foo

--copied from Idris2 source
export
data Ref : (l : label) -> Type -> Type where
     [search l]
	   MkRef : IORef a -> Ref x a

export
newRef : (x : label) -> t -> IO (Ref x t)
newRef x val
    = do ref <- newIORef val
         pure (MkRef ref)

export %inline
get : (x : label) -> {auto ref : Ref x a} -> IO a
get x {ref = MkRef io} = readIORef io

export %inline
put : (x : label) -> {auto ref : Ref x a} -> a -> IO ()
put x {ref = MkRef io} val = writeIORef io val
--end copy

-- from repl run with:
--   :exec test
test : IO ()
test =
  do 
    newRef (MkFoo) (the Int 1)
    putStrLn ("foo is " ++ show !(get (MkFoo)))
    put (MkFoo) 2
    putStrLn ("foo is now " ++ show !(get (MkFoo)))
    pure ()
    
-- co : won't work unless newRef is called (even though it is within test, Idris somehow "knows" this")    
-- test2 : IO ()
-- test2 =
--   do 
--     putStrLn ("foo is " ++ show !(get (MkFoo)))
--     put (MkFoo) 2
--     putStrLn ("foo is now " ++ show !(get (MkFoo)))
--     pure ()
    

-- co: still doesn't work, even though its defined in test, I guess the knowledge doesn't go up the call chain    
-- test3 : IO ()
-- test3 =
--   do 
--     test
--     putStrLn ("foo is " ++ show !(get (MkFoo)))
--     put (MkFoo) 2
--     putStrLn ("foo is now " ++ show !(get (MkFoo)))
--     pure ()
    
-- co: again doesn't work, it won't go down the call stack either, must need to be defined in context (even though in test, newRef isn't set to any kind of variable
-- innerTest5 : IO ()
-- innerTest5 = 
--   do
--     put (MkFoo) 2
--     putStrLn ("foo is now " ++ show !(get (MkFoo)))
--     pure ()

    
-- test5 : IO ()
-- test5 =
--   do 
--     newRef (MkFoo) (the Int 1)
--     putStrLn ("foo is " ++ show !(get (MkFoo)))
--     innerTest5

-- by specifying %hint, auto will find it (otherwise test6 won't compile)
%hint    
test6_42 : Ref 'u' Int
test6_42 = unsafePerformIO (newRef 'u' 42)

test6 : IO ()
test6 =
 do
    put 'u' 2  --this has no effect (always 42)
    putStrLn ("foo is " ++ show !(get 'u'))
    put 'u' 3 -- neither does this (always 42)
    putStrLn ("foo is now " ++ show !(get 'u'))
    pure ()
