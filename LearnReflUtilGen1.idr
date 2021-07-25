module LearnReflUtilGen1

-- import public Language.Reflection.Pretty
-- import public Language.Reflection.Syntax
-- import public Language.Reflection.Types

-- %language ElabReflection

||| An n-ary product type. This is of course
||| identical to `HVect`, unlike the version in
||| generic-sop, which is kind-polymorphic and parameterized
||| by an additional type constructor.
public export
data NP : (ts : List Type) -> Type where
  Nil : NP []
  (::) : (val : t) -> (vals : NP ts) -> NP (t :: ts)

||| An n-ary sum of products.
public export
data SOP : (tss : List (List Type)) -> Type where
  Z : NP ts   -> SOP (ts :: tss)
  S : SOP tss -> SOP (ts :: tss)

||| Witness that all elements in a list of types have
||| implementations of the given interface.
public export
All : (f : Type -> Type) -> (ts : List Type) -> Type
All f [] = ()
All f (t::ts) = (f t, All f ts)

||| Witness that all elements in a list of lists of types have
||| implementations of the given interface.
public export
All2 : (f : Type -> Type) -> (tss : List(List Type)) -> Type
All2 f [] = ()
All2 f (ts::tss) = (All f ts, All2 f tss)

public export
All Eq ts => Eq (NP ts) where
  Nil        == Nil        = True
  (h1 :: t1) == (h2 :: t2) = h1 == h2 && t1 == t2

public export
All2 Eq tss => Eq (SOP tss) where
  Z v1 == Z v2 = v1 == v2
  S v1 == S v2 = v1 == v2
  _    == _    = False

public export
All Eq ts => All Ord ts => Ord (NP ts) where
  compare Nil Nil               = EQ
  compare (h1 :: t1) (h2 :: t2) = compare h1 h2 <+> compare t1 t2

public export
All2 Eq tss => All2 Ord tss => Ord (SOP tss) where
  compare (Z v1) (Z v2) = compare v1 v2
  compare (S v1) (S v2) = compare v1 v2
  compare (Z _ ) (S _)  = LT
  compare (S _ ) (Z _)  = GT

public export
interface Generic (0 t : Type) (0 code : List (List Type)) | t where
  from : t -> SOP code
  to   : SOP code -> t

public export
genEq : Generic t code => All2 Eq code => t -> t -> Bool
genEq a b = from a == from b

public export
genCompare :  Generic t code
           => All2 Eq code
           => All2 Ord code
           => t -> t -> Ordering
-- We don't use `comparing` here, since for the time being
-- it is not publicly exported, which makes type-level
-- testing more difficult
genCompare t1 t2 = compare (from t1) (from t2)

public export
record Person where
  constructor MkPerson
  name     : String
  age      : Int
  children : List Person

public export
Generic Person [[String,Int,List Person]] where
  from (MkPerson n a cs) = Z [n,a,cs]
  to (Z [n,a,cs]) = MkPerson n a cs

export
Eq Person where (==) = genEq

export
Ord Person where compare = genCompare

public export
data ParseErr = EOF
              | ReadErr Int Int String
              | UnmatchedParen Int Int

public export
Generic ParseErr [[],[Int,Int,String],[Int,Int]] where
  from EOF                  = Z []
  from (ReadErr r c msg)    = S $ Z [r,c,msg]
  from (UnmatchedParen r c) = S $ S $ Z [r,c]

  to (Z [])            = EOF
  to (S (Z [r,c,msg])) = ReadErr r c msg
  to (S (S (Z [r,c]))) = UnmatchedParen r c

export
Eq ParseErr where (==) = genEq

export
Ord ParseErr where compare = genCompare
