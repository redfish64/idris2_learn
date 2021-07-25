module LearnReflUtil

import Language.Reflection
import Language.Reflection.Syntax

%language ElabReflection

testDecl : List Decl
testDecl = `[ --export %inline
              test : Int -> Int
              test n = n + n ]

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

enumDecl1 : (name : String) -> (cons : List String) -> Decl
enumDecl1 name cons = IData EmptyFC Public dat
  where enumName : Name
        enumName = UN name

        mkCon : String -> ITy
        mkCon n = MkTy EmptyFC EmptyFC (UN n) (IVar EmptyFC enumName)

        dat : Data
        dat = MkData EmptyFC enumName (IType EmptyFC) [] (map mkCon cons)
        
export
enumDecl : (name : String) -> (cons : List String) -> Decl
enumDecl name = simpleData Public (UN name) . map mkCon
  where mkCon : String -> ITy
        mkCon n = mkTy (UN n) (varStr name)

export
mkEnum : (name : String) -> (cons : List String) -> Elab ()
mkEnum name cons = declare [enumDecl name cons]

%runElab (mkEnum "Gender" ["Female","Male","NonBinary"])

export
Eq Gender where
  Female     == Female     = True
  Male       == Male       = True
  NonBinary  == NonBinary  = True
  _          == _          = False
