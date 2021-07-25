
import Data.IOArray

data Foo = FooI Int

Show Foo where
  show (FooI v) = "(FooI " ++ show v ++ ")"

ARR_SIZE : Int
ARR_SIZE = 10

main : IO ()
main =
  do
    x <- newArray ARR_SIZE
    for_ [0..ARR_SIZE - 1] (\i => writeArray x i (FooI $ i+50))
    mr <- traverse (\i => readArray x i) [0..ARR_SIZE-1]
    putStrLn $ show mr
    pure ()
    
