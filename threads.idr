module Main

import System.Concurrency.Raw
import System
import Data.IORef

foo : IO ()
foo = putStrLn "bla"

main : IO ()
main =
  do
    putStrLn "Done!"
