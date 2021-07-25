import System

-- see here that we have true polymorphism here (as far as I understand the definition of).  inType is not
-- known (supposedly), yet the right "show" is being called in test, below (show Char vs show Int)
data Foo : (outType : Type) -> Type where
  Foo_ : {0 inType : Type} -> Show inType => (inVal : inType) -> (outVal : outType) -> Foo outType
 
fooStuff : Foo ot -> String
fooStuff (Foo_ inVal outVal) = show inVal


test : IO ()
test =
   -- here we see that when f1 is created, a lambda of the apprpriate show function is
   -- added *as a value* to the vector
   -- (define Main-test
   --   (lambda
   --       ()
   --     (let
   --         ((u--f1
   --           (vector
   --            (cons
   --             (lambda --this is the lambda that defines how to show the value
   --      	   (u--x)
   --      	 (PreludeC-45Show-u--show_Show_Int u--x))
   --             (lambda
   --      	   (u--d)
   --      	 (lambda
   --      	     (u--x)
   --      	   (PreludeC-45Show-u--showPrec_Show_Int u--d u--x))))
   --            1 2)))
  let f1 : Foo Int = Foo_ (the Int 1) 2
      f2 : Foo Int = Foo_ (the Char 'c') 2
      vx = [f1,f2]
  in
    do
      putStrLn $ "f1 is " ++ fooStuff f1
      putStrLn $ "f2 is " ++ fooStuff f2
      putStrLn $ "vx is " ++ (show $ map fooStuff vx)
     
      --proof that it's completely dynamic 
      args <- getArgs
      let vx2 = if (length args == 0) then [f1,f1] else [f2,f2]
      putStrLn $ "vx2 is " ++ (show $ map fooStuff vx2)

--same as Foo, but now we say that (newly named) "shwy" shouldn't be available at runtime
data Foo2 : (outType : Type) -> Type where
  Foo2_ : {0 inType : Type} -> {auto 0 shwy : Show inType} -> (inVal : inType) -> (outVal : outType) -> Foo2 outType

-- co: Won't typecheck, because we forced "shwy" to be 0. 
-- foo2Stuff : Foo2 ot -> String
-- foo2Stuff (Foo2_ inVal outVal) = show inVal

-- so really, when we do something like "Show inType =>" in "Foo" were actually doing the following. Only
-- difference is the compiler is automatically populating "show" for us.
data Foo3 : (outType : Type) -> Type where
  Foo3_ : {0 inType : Type} -> {show : inType -> String} -> (inVal : inType) -> (outVal : outType) -> Foo3 outType
 
