import Control.Monad.Reader

public export
data Audit : (v : Type) -> Type where
  Const : String -> v -> Audit v -- constant value
  FromSource : String -> Loc -> v -> Audit v --read directly from a file
  Link : Audit a -> Audit b -> String -> Audit b --the result depends on "a" items somehow, but not by
                                                       --running a function. "b" is the actual value, and is
                                                       --an AuditOP so links can be chained.
  Func : (a -> b) -> (name : String) -> Audit (a -> b)
  Apply : Audit (a -> b) -> Audit a -> (cachedResult : Audit b) -> Audit b
  Write : (keyName : String) -> (keyVal : String) -> Audit resType -> Audit resType --look up a key within the internal master record
  Label : String -> Audit v -> Audit v

record Foo where
  constructor MkFoo
  fee : Int
  {0 feeAud : Audit fee}
  
record Bar where
  constructor MkBar
  bee : Int
  {auto 0 beeAud : Audit bee}
 
-- addAudit : {x,y : Int} -> Audit x -> Audit y -> Audit (x + y)
-- addAudit a b = ?xxxx --Apply ?xxx b --(Apply (Func (+) "(+)") a) b

%hint
negAudit : {x : Int} -> Audit x -> Audit (negate x)
negAudit a = Apply (Func negate "neg") a

%hint
addAudit : {x,y : Int} -> Audit x -> Audit y -> Audit (x + y)
addAudit a b = Apply (Apply (Func (+) "(+)") a) b

test1 : Bar
test1 =
   MkBar (foo.fee + foo2.fee) {beeAud=addAudit foo.feeAud foo2.feeAud}
  where
      foo : Foo 
      foo = MkFoo 1 {feeAud=Const "foo"}
      foo2 : Foo 
      foo2 = MkFoo 2 {feeAud=Const "foo2"}
      
test2 : Foo -> Foo -> Bar
test2 (MkFoo fee {feeAud=feeAud1}) (MkFoo fee2 {feeAud=feeAud2}) = 
  -- MkBar (fee + fee2) {beeAud=addAudit _ _}
  -- MkBar (fee + fee2) {beeAud=addAudit feeAud1 feeAud2}
  MkBar (fee + fee2) {beeAud= ?xxx}

public export
data HVectToT : {c : Type -> Type} -> Vect k Type -> Type where
  Nil : HVectToT []
  (::) : c t -> HVectToT {c} ts -> HVectToT {c} (t :: ts)
  
AuditT : (readerDataElems : Vect n Type ) -> (Type -> Type) -> Type -> Type
AuditT = ReaderT (HVectToT {c=AuditConst} readerDataElems


