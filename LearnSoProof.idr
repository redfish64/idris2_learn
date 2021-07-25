import System as S

||| if v < 0, should return 0, else v
forcePositiveWithPrf : (v : Int) -> (x : Int ** (So (x >= 0)))
forcePositiveWithPrf v with choose (v >= 0)
  forcePositiveWithPrf v | True = (v ** ?h)
  forcePositiveWithPrf v | False = (0 ** Oh)

||| sleeps given number of microseconds, unless less than zero, in which case sleeps 0
usleepNat : Int -> IO ()
usleepNat v = 
  let (rv ** p) = forcePositiveWithPrf v
  in
    S.usleep rv
