import Language.Reflection
import Language.Reflection.TT

import LearnDecEq


%language ElabReflection

getOnlyType : Name -> Elab (Name,TTImp)
getOnlyType tn =
  do
    ((n,t) :: Nil) <- getType tn
      | ntList => fail (show tn ++ " resolves to more than one type, " ++ show (map fst ntList))
    pure (n,t)
  
  
getRecordCons : Name -> Elab Name
getRecordCons tn =
  do
    (n,_) <- getOnlyType tn
    (c :: Nil) <- getCons n
      | cons => fail (show n ++ " has more than one constructor: " ++ show cons)
    pure c

-- %macro
-- foo : Elab (List (Name, TTImp))
-- foo = getRecordCons `{{decEqAqua2}}

data Fee = MkFee Int

foo2 : Int
foo2 = let z : Fee -> Int
           z = (\x@(MkFee v) => v)
       in z (MkFee 6)    

foo3 : TTImp
foo3 = `(\x@(MkFee v) => v)

foo4 : TTImp -> TTImp
foo4 v = `(~v + ~v)


-- foo5 : TTImp -> TTImp
-- foo5 v = doit
--   where
--     doit : TTImp
--     doit = `(let x : (~v ~v)
--              in x)
    
foo6 : TTImp -> TTImp
foo6 v = `(let x : (~v ~v)
           in x)


-- foo7 : TTImp
-- foo7 = `(let x : (~foo3 ~foo3)
--          in x)


foo8 : TTImp
foo8 = `(do 
           8 7
           9 10
           11 12
           )

foo9 : TTImp
foo9 = `(1 + 2)

foo10 : Int
foo10 = %runElab (check foo9)

-- does not work
-- foo11 : TTImp -> Int
-- foo11 x = %runElab (check x)

-- foo11 : TTImp
-- foo11 = `((vt : Type ** (v : vt)))
