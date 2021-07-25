module LearnReflectionElabReadFile

import Language.Reflection
import Language.Reflection.TT

%language ElabReflection

foo : Elab String
foo = readFile "LearnReflectionElabReadFile.idr"

foo2 : String
foo2 = %runElab foo


