import Language.Reflection
import Language.Reflection.TT

%language ElabReflection

%macro
Unique : Type -> Elab Type
Unique x = 
  do 
    v <- quote x
    check v

-- uniqueZero : Unique (n : Nat ** n + n = n)

%macro
Foo : Type -> Elab TTImp
Foo x = 
  do 
    v <- quote x
    pure v
    
foo : TTImp
foo = Foo (n : Nat ** n + n = n)


one : TTImp
one = `(1)

-- sillyMatch : (v : TTImp) -> TTImp
-- sillyMatch v = 
--   `(case ~v of
--             ~one => True
--             _ => False)

list : List Int
list = [ 
         1 ,
         2 ,
         3
       ]
    
sillyMatch : TTImp -> TTImp
sillyMatch v = ICase EmptyFC v `(Int) 
                 [ 
                   PatClause EmptyFC `(1) `(True),
                   PatClause EmptyFC (Implicit EmptyFC False) `(False) 
                 ]
                 

%macro
sillyMatch' : Int -> Elab Bool
sillyMatch' v = 
   do 
      qv <- quote v
      check $ sillyMatch qv

sillyMatch2 : TTImp -> TTImp -> TTImp
sillyMatch2 v c = ICase EmptyFC v `(Int) 
                 [ 
                   PatClause EmptyFC c `(True),
                   PatClause EmptyFC (Implicit EmptyFC False) `(False) 
                 ]
                 

%macro
sillyMatch2' : Int -> Int -> Elab Bool
sillyMatch2' v c = 
   do 
      qv <- quote v
      qc <- quote c
      check $ sillyMatch2 qv qc

sillyMatch3 : TTImp -> TTImp -> TTImp -> TTImp
sillyMatch3 t v c = ICase EmptyFC v t
                 [ 
                   PatClause EmptyFC c `(True),
                   PatClause EmptyFC (Implicit EmptyFC True) `(False) 
                 ]
               
data WeDontDoEq = Tree | Cat                   

%macro
sillyMatch3' : {t : Type} -> (v1 : t) -> (v2 : t) -> Elab Bool
sillyMatch3' {t=t} v c = 
   do 
      qt <- quote t
      qv <- quote v
      qc <- quote c
      check $ sillyMatch3 qt qv qc

testSillyMatch3 : Bool
testSillyMatch3 = sillyMatch3' Tree Tree

testSillyMatch3' : Bool
testSillyMatch3' = sillyMatch3' Tree Cat

-- testSillyMatch3'' : WeDontDoEq -> WeDontDoEq -> Bool
-- testSillyMatch3'' x y = sillyMatch3' x y


sillyMatch4 : TTImp -> TTImp -> TTImp -> TTImp
sillyMatch4 t v c = ICase EmptyFC v t
                 [ 
                   PatClause EmptyFC c `(True),
                   PatClause EmptyFC (Implicit EmptyFC True) `(False) 
                 ]
               
%macro
sillyMatch4' : {t : Type} -> (v1 : t) -> (qc : TTImp) -> Elab Bool
sillyMatch4' {t=t} v qc = 
   do 
      qt <- quote t
      qv <- quote v
      check $ sillyMatch3 qt qv qc

-- %macro
-- testSillyMatch4 : WeDontDoEq -> TTImp -> Bool
-- testSillyMatch4 x y = sillyMatch4' x y

foothing : TTImp
foothing = `(\v => case v of 
                       'c' => True
                       _ => False)

data Funky = FooFunk | BarFunk

foofunc : List Decl
foofunc = `[
            %hint
            x : Int
            x = 42]
           
-- Eq Funky where
--   (==) x y = True

-- the result of C-c C-c
-- v : Eq Funky -> ()
-- v (Eq at Prelude/EqOrd.idr:13:1--22:7 == /=) = ?v_rhs_

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

v : Eq Funky
v = %runElab (do
                cnstrName <- getRecordCons `{{Eq}}
                let cnstrVar = IVar EmptyFC cnstrName
                check `((~cnstrVar) (\x => \y => True) (\x => \y => False)))
                
%hint
v2 : Eq Funky
v2 = v
              

v3 : Bool
v3 = BarFunk == FooFunk
   
         
foofunc2 : List Decl
foofunc2 = `[
            %hint
            x : Int
            x = 42]
     
-- funkyTT : TTImp
-- funkyTT = `(Funky)          
                                       
-- foofunc2 : List Decl
-- foofunc2 = `[
--              Eq Funky where
--                (==) x y = True
--               ]

zzz : TTImp
zzz = `(Just 1)

--
-- The following was copy and pasted from Idris2/tests/idris2/reg033/DerivingEq.idr
--
public export
countArgs : (ty : TTImp) -> Nat
countArgs (IPi _ _ ExplicitArg _ _ retTy) = 1 + countArgs retTy
countArgs (IPi _ _ _ _ _ retTy) = countArgs retTy
countArgs _ = 0

public export
genEq : Name -> Elab TTImp --Elab (t -> t -> Bool)
genEq typeName = do
  let pos : FC = MkFC "generated code" (0,0) (0,0)
  [(n, _)] <- getType typeName
      | _ => fail "Ambiguous name"
  constrs <- getCons n
  let and : TTImp -> TTImp -> TTImp
      and x y = `(~(x) && ~(y))
      compareEq : String -> String -> TTImp
      compareEq x y = `(~(IVar pos $ UN x) == ~(IVar pos $ UN y))
      makeClause : Name -> Elab Clause
      makeClause constr = do
        [(_, ty)] <- getType constr
            | _ => fail "ambiguous name for constr"
        let nArgs = countArgs ty
        let xs = map (\i => "x_" ++ show i) $ take nArgs [1..]
        let ys = map (\i => "y_" ++ show i) $ take nArgs [1..]
        let px = foldl (IApp pos) (IVar pos constr) $ map (IBindVar pos) xs
        let py = foldl (IApp pos) (IVar pos constr) $ map (IBindVar pos) ys
        pure $ PatClause pos `(MkPair ~(px) ~(py))
             $ foldl and `(True) $ zipWith compareEq xs ys
      finalClause : Clause
      finalClause = PatClause pos `(_) `(False)
  clauses <- traverse makeClause constrs
  let allClauses = clauses ++ [finalClause]
      caseExpr = ICase pos `(MkPair x y) (Implicit pos True) allClauses
      result = `(\x, y => ~(caseExpr))
  pure result
  --check result

notIt : (t -> t -> Bool) -> t -> t -> Bool
notIt f x y = not $ f x y

--www : Name -> Elab ()
www : Name -> Elab (List Decl)
www nx = let pos = MkFC "generated code" (0,0) (0,0)
         in  
            do
                   --first create a function for eq
                   --eqFn <- genSym "eqfn"
                   let eqFn = (UN "fooey") --genSym "fn"
                   genEq <- genEq nx
                   let iclaim = IClaim pos MW Public []
                                       (MkTy pos eqFn `(~(IVar pos nx) -> ~(IVar pos nx) -> Bool))
                   let idef = IDef pos eqFn [PatClause pos (IVar pos eqFn) genEq]
                   declare [iclaim,idef]
                  
                   --now create a function that has "hint" turned on, and returns an Eq
                   --this will implement the interface 
                   cnstrName <- getRecordCons `{{Eq}}
                   let cnstrVar = IVar EmptyFC cnstrName
                   
                    
                   declare `[%hint
                             funcName : Eq ~(IVar pos nx)
                             funcName = ~(IVar pos cnstrName) ~(IVar pos eqFn) (notIt ~(IVar pos eqFn))
                            ]
                   
                   pure `[%hint
                          funcName : Eq ~(IVar pos nx)
                          funcName = ~(IVar pos cnstrName) ~(IVar pos eqFn) (not ~(IVar pos eqFn))
                         ]
                   
                   -- pure `[funcName : ~(IVar pos nx) -> ~(IVar pos nx) -> Bool
                   --        funcName = ~(genEq)]
                   -- declare `[funcName : ~(IVar pos nx) -> ~(IVar pos nx) -> Bool
                   --           funcName = ~(genEq)]
                             -- (notIt ~(genEq `{{Funky}}))]
                   -- pure ()

data Flippy = Dolphin | Shark

www' : List Decl
www' = %runElab (www `{{Flippy}})

testwww : Bool
testwww = Dolphin == Shark

--[
-- *** this is the type of whatever is next (in this case, a functon) ***
-- IClaim fc MW Private [fnopts]
--     *** this is ITy which defines the name of what is being defined, and it's type *** 
--    (MkTy fc (UN "funcName") (IPi fc MW ExplicitArg (Maybe <arg name>) (arg type) (ret type)))
-- ,
-- *** This is the body of the function (could be something else, such as IData, I guess ***
-- IDef fc (UN "funcName")
--    *** list of pattern clauses (and that's it) *** 
--    [PatClause fc (IVar fc (UN "funcName")) (ILam fc MW ExplicitArg (Just (UN "x")) (Implicit fc False) (ILam fc MW ExplicitArg (Just (UN "y")) (Implicit fc False) (ICase fc (IApp fc (IApp fc (IVar fc (UN "MkPair")) (IVar fc (UN "x"))) (IVar fc (UN "y"))) (Implicit fc True) [PatClause fc (IApp fc (IApp fc (IVar fc (UN "MkPair")) (IVar fc (NS ["Main"] (UN "FooFunk")))) (IVar fc (NS ["Main"] (UN "FooFunk")))) (IVar fc (UN "True")), PatClause fc (IApp fc (IApp fc (IVar fc (UN "MkPair")) (IVar fc (NS ["Main"] (UN "BarFunk")))) (IVar fc (NS ["Main"] (UN "BarFunk")))) (IVar fc (UN "True")), PatClause fc (Implicit fc True) (IVar fc (UN "False"))])))]]

-- w : Eq Funky
-- w = %runElab (do
--                 cnstrName <- getRecordCons `{{Eq}}
--                 let cnstrVar = IVar EmptyFC cnstrName
--                 check `((~cnstrVar) ~(genEq `{{Funky}}) (notIt ~(genEq `{{Funky}}))))


-- data TreeOne a = BranchOne (TreeOne a) a (TreeOne a)
--                | Leaf

-- Eq a => Eq (TreeOne a) where
--   (==) = %runElab genEq `{{ TreeOne }}


-- mkEqFunc : List Decl
-- foofunc = `[
--             %hint
--             x : Int
--             x = 42]
