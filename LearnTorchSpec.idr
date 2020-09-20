module LearnTorchSpec

-- experiments in making torch spec stuff

--special stateful bind

data SpecStateKind = SSInit | SSIsAbs | SSName | SSCName | SSCons

data MoveFromSpecStateKinds : SpecStateKind -> SpecStateKind -> Type where
  m1 : MoveFromSpecStateKinds SSInit SSIsAbs
  m2 : MoveFromSpecStateKinds SSInit SSName
  m2_1 : MoveFromSpecStateKinds SSIsAbs SSName
  m3 : MoveFromSpecStateKinds SSName SSCName
  m3_1 : MoveFromSpecStateKinds SSName SSCons
  m4 : MoveFromSpecStateKinds SSCName SSCons

record SpecStateData where
  constructor MkSpecStateData
  isAbs : Bool
  name : String
  cname : String
  cons : List String --TODO
  

data SpecState : SpecStateKind -> a -> Type where
  MkSpecState : (0 x : SpecStateKind) -> (v : a) -> SpecState x a
 
myBind : SpecState x a -> (a -> SpecState y b) -> {auto 0 _ : MoveFromSpecStateKinds x y} -> SpecState y b
myBind (MkSpecState _ v) f = f v

--works but we can cheat
test : SpecState SSName Int
test = let (>>=) = myBind in
         (do
            MkSpecState SSInit 1
            MkSpecState SSIsAbs 2
            MkSpecState SSName 3
            )
           
testCheat : SpecState SSName Int
testCheat = let (>>=) = myBind in
         (do
            --we can specify no init, and its not enforced
            MkSpecState SSIsAbs 2
            MkSpecState SSName 3
            )
           
