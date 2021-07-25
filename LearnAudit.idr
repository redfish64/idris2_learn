
public export
data Audit : {t : Type} -> (v : t) -> Type where
  [noHints]
  Const : String -> Audit v -- constant value
  FromSource : String -> Audit v --read directly from a file
  Link : Audit a -> Audit b -> String -> Audit b --the result depends on "a" items somehow, but not by
                                                       --running a function. "b" is the actual value, and is
                                                       --an AuditOP so links can be chained.
  Func : (f : at -> bt) -> (name : String) -> Audit f
  -- Apply : Audit z
  Apply : {f : at -> bt} -> Audit f -> {a : at} -> Audit a -> Audit (f a)
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
