module LearnReflUtil2

import LearnReflUtil
import Language.Reflection
import Language.Reflection.Syntax
import public Language.Reflection.Types

%language ElabReflection


export
eqDecl1 : String -> List String -> List Decl
eqDecl1 enumName cons =
  let functionName = UN $ "impl1Eq" ++ enumName
      function     = var functionName
      enum         = arg $ varStr enumName

      -- Default clause for non-matching constructors:
      -- `function _ _ = False`
      defClause    = function .$ implicitTrue .$ implicitTrue .= `(False)

      -- Pattern clause for a single constructor:
      -- `function A A = True`
      conClause    = \c => function .$ varStr c .$ varStr c .= `(True)

   in [ public' functionName $ enum .-> enum .-> `(Bool)
      , def functionName $ map conClause cons ++ [defClause] ]

export
mkEq1 : String -> List String -> Elab ()
mkEq1 n cons = declare $ eqDecl1 n cons

%runElab (mkEq1 "Gender" ["Female","Male","NonBinary"])

eqTest : impl1EqGender Female Female = True
eqTest = Refl

export
eqInfo : TypeInfo
eqInfo = getInfo "Eq"

export
eqImpl : String -> List String -> List Decl
eqImpl enumName cons =
  let -- names
      mkEq         = singleCon "Eq"
      eqName       = UN "eq"
      functionName = UN $ "implEq" ++ enumName

      -- vars
      eq           = var eqName
      function     = var functionName
      enum         = arg $ varStr enumName

      -- Catch all case: eq _ _ = False
      defEq = eq .$ implicitTrue .$ implicitTrue .= `(False)

      -- single pattern clause: `eq X X = True`
      mkC   = \x => eq .$ varStr x .$ varStr x .= `(True)

      -- implementation of (/=)
      neq = `(\a,b => not $ eq a b)

      -- local where block:
      -- ... = EqConstructor eq neq
      --   where eq : Enum -> Enum -> Bool
      --         eq A A = True
      --         ...
      --         eq _ _ = False
      impl  = local [ private' eqName $ enum .-> enum .-> `(Bool)
                    , def eqName $ map mkC cons ++ [defEq]
                    ] (var mkEq .$ eq .$ neq)

   in [ interfaceHint Public functionName (var "Eq" .$ type enum)
      , def functionName [ function .= impl ] ]
      
export
mkEqImpl : String -> List String -> Elab ()
mkEqImpl enumName cons = declare (eqImpl enumName cons)

%runElab (mkEqImpl "Gender" ["Female","Male","NonBinary"])

eqTest2 : (Male == NonBinary) = False
eqTest2 = Refl

eqTest3 : (Male /= NonBinary) = True
eqTest3 = Refl
