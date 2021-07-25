import Language.Reflection
import Decidable.Equality

%language ElabReflection

foo : TTImp
foo = `(Z)

foo2 : TTImp
foo2 = `(Int -> Int)

foo3 : TTImp
foo3 = `((1 x : Int) -> Int)

foo4 : TTImp
foo4 = `((0 x : Int) -> Int)

--"IPrimVal"
foo5: TTImp
foo5 = `("Foo")

--special "IType"
foo6: TTImp
foo6 = `(Type)

--this is "IVar", just like Z
foo7: TTImp
foo7 = `(TTImp)

oneOrZero : Maybe () -> Int
oneOrZero (Just ()) = 1
oneOrZero Nothing = 0

foo8 : TTImp
foo8 = `(oneOrZero)

names : List Name
names = [ `{{ namesfdafds }}, `{{ Prelude.(+) }} ]

-- data Flippy = Dolphin | Shark

-- DecEq Flippy where
--   decEq Dolphin Dolphin = Yes Refl
--   decEq a@(Dolphin) b@(Shark) = 
--     let foo : (a = b) -> Void
--         foo Refl impossible
--     in No foo
--   -- decEq Dolphin Shark = No (\x => case x of Refl impossible)
--   decEq Shark Dolphin = No (\x => case x of Refl impossible)
--   decEq Shark Shark = Yes Refl

-- data Aqua = MkAqua Flippy Flippy

-- decEqFn : (x1 : Aqua) -> (x2 : Aqua) -> Dec (x1 = x2)
-- decEqFn (MkAqua a b) (MkAqua a' b') = 
--   let 
--     aquaEqFirstArgEq : {a,b,c,d : _ } -> (MkAqua a b) = (MkAqua c d) -> a = c
--     aquaEqFirstArgEq Refl = Refl
--     aquaEqSndArgEq : (MkAqua a b) = (MkAqua c d) -> b = d
--     aquaEqSndArgEq Refl = Refl
--   in 
--     case (decEq a a') of 
--        (No contra) => No $ contra . aquaEqFirstArgEq
--        (Yes prf) => case (decEq b b') of 
--                       (No contra) => No $ contra . aquaEqSndArgEq
--                       (Yes prf2) => Yes (rewrite (sym prf2) in (rewrite (sym prf) in Refl))

-- foo9 : TTImp
-- foo9 = `(let aquaEqFirstArgEq : {a,b,c,d : _ } -> (MkAqua a b) = (MkAqua c d) -> a = c
--              aquaEqFirstArgEq Refl = Refl
--          in 42)

-- -- *** foo9 result: ***
-- -- LOG 1: : (%local ([[] (%claim aquaEqFirstArgEq (%pi RigW Implicit (Just a) _ (%pi RigW Implicit (Just b) _ (%pi RigW Implicit (Just c) _ (%pi RigW Implicit (Just d) _ (%pi RigW Explicit Nothing (|((=== ((MkAqua a) b)) ((MkAqua c) d)),((~=~ ((MkAqua a) b)) ((MkAqua c) d))|) (|((=== a) c),((~=~ a) c)|))))))), (%def aquaEqFirstArgEq [(aquaEqFirstArgEq Refl) = Refl])]) (fromInteger 42))
-- -- ILocal f (68, 11)) 
-- --    [IClaim f MW Private [] 
-- --      (MkTy f (UN "aquaEqFirstArgEq") 
-- --        (IPi f MW ImplicitArg (Just (UN "a")) (Implicit f True) 
-- --          (IPi f MW ImplicitArg (Just (UN "b")) (Implicit f True) 
-- --            (IPi f MW ImplicitArg (Just (UN "c")) (Implicit f True) 
-- --              (IPi f MW ImplicitArg (Just (UN "d")) (Implicit f True) 
-- --                (IPi f MW ExplicitArg Nothing 
-- --                  (IAlternative f FirstSuccess 
-- --                    [IApp f (IApp f (IVar f (UN "===")) (IApp f (IApp f (IVar f (UN "MkAqua")) (IVar f (UN "a"))) (IVar f (UN "b")))) (IApp f (IApp f (IVar f (UN "MkAqua")) (IVar f (UN "c"))) (IVar f (UN "d"))), 
-- --                     IApp f (IApp f (IVar f (UN "~=~")) (IApp f (IApp f (IVar f (UN "MkAqua")) (IVar f (UN "a"))) (IVar f (UN "b")))) (IApp f (IApp f (IVar f (UN "MkAqua")) (IVar f (UN "c"))) (IVar f (UN "d")))]) 
-- --                  (IAlternative f FirstSuccess 
-- --                    [IApp f (IApp f (IVar f (UN "===")) (IVar f (UN "a"))) (IVar f (UN "c")), 
-- --                     IApp f (IApp f (IVar f (UN "~=~")) (IVar f (UN "a"))) (IVar f (UN "c"))]))
-- --                 ))))), 
-- --     IDef f (UN "aquaEqFirstArgEq") 
-- --       [PatClause f (IApp f (IVar f (UN "aquaEqFirstArgEq")) (IVar f (UN "Refl"))) (IVar f (UN "Refl"))]
-- --    ] 
-- --    (IApp f (IVar f (UN "fromInteger")) (IPrimVal f (BI 42)))

-- %logging 1

-- foo10 : Elab ()
-- foo10 = logTerm "" 1 "" foo9

-- %macro
-- foo11 : Elab ()
-- foo11 = foo10

-- %language ElabReflection

-- foo12 : ()
-- foo12 = foo11

-- quickShow : TTImp -> String
-- quickShow (IVar _ n) = "IVar " ++ show n
-- quickShow (IPi _ _ _ _ _ _) = "IPi"
-- quickShow (ILam _ _ _ _ _ _) = "ILam"
-- quickShow (INamedApp _ _ _ _) = "INamedApp"
-- quickShow (IAutoApp _ _ _) = "IAutoApp"
-- quickShow (ILet _ _ _ _ _ _ _) = "ILet"
-- quickShow (ICase _ _  _ _) = "ICase"
-- quickShow (ILocal _ _ _) = "ILocal"
-- quickShow (IUpdate _ _ _) = "IUpdate"

-- quickShow (IApp _ x y) = "IApp (" ++ quickShow x ++ ") (" ++ quickShow y ++ ")" 
-- quickShow (IWithApp _ _ _) = "IWithApp"

-- quickShow (ISearch _ _) = "ISearch"
-- quickShow (IAlternative _ _ _) = "IAlternative"
-- quickShow (IRewrite _ _ _) = "IRewrite"

--        -- Any implicit bindings in the scope should be bound here, using
--        -- the given binder
-- quickShow (IBindHere _ _ _) = "IBindHere"
--        -- A name which should be implicitly bound
-- quickShow (IBindVar _ _) = "IBindVar"
--        -- An 'as' pattern, valid on the LHS of a clause only
-- quickShow (IAs _ _ _ _ _) = "IAs"
--        -- A 'dot' pattern, i.e. one which must also have the given value
--        -- by unification
-- quickShow (IMustUnify _ _ _) = "IMustUnify"

--        -- Laziness annotations
-- quickShow (IDelayed _ _ _) = "IDelayed"
-- quickShow (IDelay _ _) = "IDelay"
-- quickShow (IForce _ _) = "IForce"

--        -- Quasiquotation
-- quickShow (IQuote _ _) = "IQuote"
-- quickShow (IQuoteName _ _) = "IQuoteName"
-- quickShow (IQuoteDecl _ _) = "IQuoteDecl"
-- quickShow (IUnquote _ _) = "IUnquote"

-- quickShow (IPrimVal _ _) = "IPrimVal"
-- quickShow (IType _) = "IType"
-- quickShow (IHole _ _) = "IHole"

--        -- An implicit value, solved by unification, but which will also be
--        -- bound (either as a pattern variable or a type variable) if unsolved
--        -- at the end of elaborator
-- quickShow (Implicit _ _) = "Implicit"
-- quickShow (IWithUnambigNames _ _ _) = "IWithUnambigNames"

-- foo13 : TTImp
-- foo13 = `(vt : Type ** (v : vt))
