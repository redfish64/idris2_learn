module LearnTorchSpec2

-- experiments in making torch spec stuff

--special stateful bind

data SpecStateKind = SSInit | SSIsAbs | SSName | SSCName | SSCons | SSFin

data MoveFromSpecStateKinds : SpecStateKind -> SpecStateKind -> Type where
  m1 : MoveFromSpecStateKinds SSInit SSIsAbs
  --m2 : MoveFromSpecStateKinds SSInit SSName
  m2_1 : MoveFromSpecStateKinds SSIsAbs SSName
  m3 : MoveFromSpecStateKinds SSName SSCName
  m3_1 : MoveFromSpecStateKinds SSName SSCons
  m4 : MoveFromSpecStateKinds SSCName SSCons
  m5 : MoveFromSpecStateKinds SSCons SSFin
  --m5_1 : MoveFromSpecStateKinds SSName SSFin

record SpecStateData where
  constructor MkSpecStateData
  isAbs : Bool
  name : String
  cname : String
  cons : List String --TODO
  

data SpecState : {ssk : Type} -> (buildLaws : ssk -> ssk -> Type) -> ssk -> a -> Type where
  MkInitSpecState : (v : a) -> SpecState buildLaws SSInit a
  MkSpecState : (curKind : ssk) -> {prevKind : ssk} -> {auto 0 _ : buildLaws prevKind curKind} -> (v : a) -> (0 prev : SpecState buildLaws prevKind xxx ) -> SpecState buildLaws curKind a
 
myBind : ((0 _ : (SpecState buildLaws prevKind prevVal)) -> SpecState buildLaws x a) -> (a -> ((0 _ : SpecState buildLaws x a) -> SpecState buildLaws y b)) -> ((0 _ : SpecState buildLaws prevKind prevVal) -> SpecState buildLaws y b)
myBind f g x = 
  let fxr = (f x) 
  in
    case fxr of
      (MkInitSpecState v) => g v fxr
      (MkSpecState _ v _) => g v fxr

-- myBind prev@(MkInitSpecState v) f = f v prev 
-- myBind prev@(MkSpecState _ v _) f = f v prev 

testRunner : ((0 _ : SpecState MoveFromSpecStateKinds SSInit Int) -> SpecState MoveFromSpecStateKinds z Int) -> {auto 0 _ : MoveFromSpecStateKinds z SSFin} -> Int
testRunner f = case f (MkInitSpecState 5) of 
                 (MkInitSpecState v) => v
                 (MkSpecState z v prev) => v
                 
test : (0 _ : SpecState MoveFromSpecStateKinds SSInit Int) -> SpecState MoveFromSpecStateKinds SSCons Int
test = let (>>=) = myBind 
       in
            -- MkSpecState SSIsAbs 5 >>=
            -- (\_ => MkSpecState SSName 5 >>=
            -- (\_ => MkSpecState SSCons 5))
         do
            MkSpecState SSIsAbs 5
            MkSpecState SSName 5
            MkSpecState SSCons 5

testTestRunner : Int
testTestRunner = testRunner test                                            

 
 
 
 
