module LearnReflection7
import Language.Reflection
import Language.Reflection.TT
import Language.Reflection.Types
import Language.Reflection

%language ElabReflection

foo : List Decl
foo = `[
record Foo where
  fee : Int
  ]
  
record Foo where
  fee : Int

namespace Foo
  public export
  xxx : Int
  xxx =6 

  -- [ INamespace Foo
  --     IClaim MW Export [] (MkTy foo2 (IPrimVal Int))
  --     IDef foo2 [PatClause (IVar foo2) (IApp. IVar fromInteger $ IPrimVal 42)] ]
foo2 : List Decl
foo2 = `[
namespace Foo
  export
  foo2 : Int
  foo2 = 42
    ]
