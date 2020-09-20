
data Var = MkVar -- Phantom, just for labelling purposes

Show Var where
  showPrec _ x = "Var"
  
data InList : vt -> List vt -> Type where
     Here : InList v (v :: vs)
     There : InList v vs -> InList v (x :: vs)

Show (InList vt vts) where
  showPrec _ Here = "Here"
  showPrec v (There x) =  "(There " ++ showPrec v x ++ ")"

%inline
%tcinline
isInList : (v : vt) -> (vs : List vt) -> {auto prf : InList v vs} -> InList v vs
isInList v vs {prf} = prf

%inline
%tcinline
foo : v -> v -> String
foo x y =
  let 
      l : List v
      l = [x,y]
  in 
    show $ "x: " ++ show (isInList x l) ++ ", "
        ++ "y: " ++ show (isInList y l)
           
--works right. but...
foo1 : String
foo1 = foo MkVar MkVar  
 
--doesn't work right.. let does something different 
foo2 : String
foo2 =
  let 
      x : Var
      x = MkVar
      y : Var
      y = MkVar
      l : List Var
      l = [x,y]
  in 
    show $ "x: " ++ show (isInList x l) ++ ", "
        ++ "y: " ++ show (isInList y l) 

--works? Why?
foo3 : String
foo3 = 
  let x = MkVar
      y = MkVar
  in
    foo x y
    
--also works
foo4 : String
foo4 = 
  let x : Var
      x = MkVar
      y : Var
      y = MkVar
  in
    foo x y

foo5 : String
foo5 = foo () ()

foo6 : String
foo6 = foo 0 0
-- --doesn't work right.. let does something different 
-- foo5 : String
-- foo5 =
--   let 
--       x = MkVar
--       y = MkVar
--       l = the List [x,y]
--   in 
--     show $ "x: " ++ show (isInList x l) ++ "\n"
--         ++ "y: " ++ show (isInList y l) ++ "\n"

