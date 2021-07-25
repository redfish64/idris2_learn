    import System.Concurrency.Raw as SC
    import System as S

    ||| returns v if positive, or 0, along with a proof that the result is positive
    forcePositiveWithPrf : (v : Int) -> (x : Int ** (So (x >= 0)))
    forcePositiveWithPrf v with (choose (v >= 0))
      forcePositiveWithPrf v | (Left p) = (v ** p)
      forcePositiveWithPrf v | _ = (0 ** Oh)

    ||| usleep, except that sleeps 0 if v < 0
    usleepEasy : Int -> IO ()
    usleepEasy v = 
      let (rv ** p) = forcePositiveWithPrf v
      in
        S.usleep rv

    ||| sleeps for delay, then writes each element and sleeps for perWriteDelay, then repeats until count is zero.
    ||| Notifies using conditionSignal when fork is finished  
    usleepAndPrint : Show a => Mutex -> Condition -> Int -> Int -> List a -> Nat -> IO ()
    usleepAndPrint mutex condition delay perWriteDelay list count = 
      do
        for_ [1..count] (const usleepAndPrint1)

        mutexAcquire mutex
        conditionSignal condition
        mutexRelease mutex
      where
        usleepAndPrint1 : IO () 
        usleepAndPrint1 = 
          do
            usleepEasy delay
            mutexAcquire mutex
            for_ list (\v => 
                          do
                            putStr (show v)
                            usleepEasy perWriteDelay
                            ) 
            putStrLn ""
            mutexRelease mutex

    MS : Int
    MS = 1000  

    main : IO ()
    main =
      do
        mutex <- makeMutex
        condition <- makeCondition

        putStrLn "hello world!"

        --create two threads each trying to print out to stdout. Using the mutex they don't interrupt each other
        fork (usleepAndPrint mutex condition (500 * MS) (20 * MS) [1..5] 3)
        fork (usleepAndPrint mutex condition (100 * MS) (30 * MS) ["apple","pear","banana"] 8)

        --wait for each fork before exiting
        mutexAcquire mutex
        conditionWait condition mutex
        conditionWait condition mutex
        mutexRelease mutex

        putStrLn "goodbye!"
        pure ()

