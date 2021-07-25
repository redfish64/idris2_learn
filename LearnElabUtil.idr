import Language.Reflection.Types
import Language.Reflection.Pretty as P
import Data.So

%language ElabReflection

namespace A
  record Foo where
    constructor Foo_
    Bar : Int
    Bee : Char


  test : ParamTypeInfo
  test = getParamInfo "Foo"

public export
data AuditType : Type where
  ATRecord : AuditType
  ATIxed : (keyType : Type) -> AuditType --some sort of indexable container, like a Vect or a SortedMap
  ATVal : AuditType

public export
AId : Type
AId = Nat
 

public export 
record Audit (0 auditType : AuditType) (0 valType : Type) where
  constructor Audit_
  aId : AId -- Audit objects have an id so that we can compare for equality. This allows us to read the auditop log and find the history of a particular audit, without making any requirements on the value
  auditData : valType

record Foo' where
  constructor Foo'_
  Bar' : Audit ATVal Int
  Bee' : Audit ATVal Char


test' : ParamTypeInfo
test' = getParamInfo "Foo'"

testio' : IO ()
testio' = putPretty test'

goo : Int -> Int
goo x = gee x
  where gee : Int -> Int
        gee = (+1)
        

testgoo : IO ()
testgoo = putPretty
  `[
 goo : Int -> Int
 goo x = gee x
   where gee : Int -> Int
         gee = (+1)
   ]
 
record Foo2 where
  constructor Foo2_
  Bar2 : Audit ATVal Int
  Bee2 : Audit ATVal Char
  {auto fooxx : So True}

test2 : TypeInfo
test2 = getInfo "Foo2"

testio2 : IO ()
testio2 = putPretty test2
