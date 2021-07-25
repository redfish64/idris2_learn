import Data.Vect

getThing : IO (w ** Vect w Nat)
getThing = pure (1 ** [4])

main : IO ()
main = do
  thing <- getThing
  putStrLn . show $ fst $ thing
  
