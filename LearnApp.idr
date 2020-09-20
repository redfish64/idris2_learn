module LearnApp

import Control.App
import Control.App.Console

hello : Console e => App e ()
hello = putStrLn " Hello , App world !"

data StateNames = Counter

helloCount : ( Console e, State Counter Int e ) => App e ()
helloCount = do c <- get Counter
                put Counter ( c + 1)
                putStrLn (show c ++ " Hello , counting world ")

wrapHelloCount : Console e => App e ()
wrapHelloCount = 
                 new 0 (do
                          helloCount
                          helloCount)

runHello : IO ()
runHello = run hello                          
                                                                              
runHello2 : IO ()
runHello2 = run wrapHelloCount                  
                                                                              
hello : Console e => App e ()
hello = putStrLn " Hello , App world !"


data Access = LoggedOut | LoggedIn
data Store : Access -> Type

-- connect : (1 k : (1 s : Store LoggedOut ) -> IO a ) -> IO a
-- disconnect : (1 s : Store LoggedOut ) -> IO ()
-- login : (1 s : Store LoggedOut ) -> ( password : String ) ->
--         Res Bool (\ ok => Store ( if ok then LoggedIn else LoggedOut ))
-- logout : (1 s : Store LoggedIn ) -> Store LoggedOut
-- readSecret : (1 s : Store LoggedIn ) ->
--              Res String ( const ( Store LoggedIn ))

interface StoreI e where
  connect : (1 prog : (1 d : Store LoggedOut ) -> App { l } e ()) -> App { l } e ()
  disconnect : (1 d : Store LoggedOut ) -> App { l } e ()
  
  login : (1 s : Store LoggedOut ) -> ( password : String ) ->
          Res Bool (\ ok => Store ( if ok then LoggedIn else LoggedOut ))
  logout : (1 s : Store LoggedIn ) -> Store LoggedOut
  readSecret : (1 s : Store LoggedIn ) ->
               Res String ( const ( Store LoggedIn ))


-- storeProg : Has [ Console , StoreI ] e = > App e ()
-- storeProg = let ( > >=) = bindL in
-- do putStr " Password : "
-- password <- getStr
-- connect $ \ s = >
-- do let True @@ s = login s password
-- | False @@ s = > do putStrLn " Wrong password "
-- disconnect s
-- let str @@ s = readSecret s
-- putStrLn $ " Secret : " ++ show str
-- let s = logout s
-- disconnect s

-- storeProg2 : Has [ Console , StoreI ] e => App e ()
-- storeProg2 = let (>>=) = bindL 
--             in
--               do putStr " Password : "
--                  password <- getLine
--                  connect doConnect
--             where 
--               doConnect : StoreI e => Store LoggedOut -> App {l} e ()
--               doConnect s =
--                              -- let loginRes : Res Bool _ = login s password 
--                              -- in 
                           
--                              -- let True @@ s = login s password
--                              --   | False @@ s => do putStrLn " Wrong password "
--                              --                      disconnect s
--                              -- let str @@ s = readSecret s
--                              -- putStrLn $ " Secret : " ++ show str
--                              -- let s = logout s
--                                  disconnect s


-- foo : IO ()
-- foo = do
--          run $ throw Void


--note that there are different PrimIO's
-- PrimIO.PrimIO : Type -> Type
-- Control.App.PrimIO : List Error -> Type
-- (initially I was confused, because PrimIO is Type -> Type, but it could convert it
-- to an App that is List Error -> ...
foo1 : (Control.App.PrimIO e) -> App e ()
foo1 pi = primIO (putStrLn "hello")
  -- ?x
    
foo2 : (Control.App.PrimIO e) -> IO ()
foo2 pi = putStrLn "hello"
  --?x
    
