
data Foo x = Feez x

data DShowIt : Type -> Type where
  DNoShow : DShowIt x
  DHasShow : Show x => DShowIt x
  
dshow : Foo x -> {auto si : DShowIt x} -> String
dshow (Feez y) {  si = DHasShow} = "Feez " ++ show y
dshow (Feez y) {  si = DNoShow} = "Feez"

data NoShow = NoS1

foo1 : Foo Int
foo1 = Feez 5

foo2 : Foo NoShow
foo2 = Feez NoS1

test1 : String
test1 = dshow foo1

test2 : String
test2 = dshow foo2


-- data Foo2 = Feez2 x

-- dshow2 : Foo2 -> {auto si : DShowIt x} -> String
-- dshow2 (Feez2 y) {  si = DHasShow} = "Feez2 " ++ show y
-- dshow2 (Feez2 y) {  si = DNoShow} = "Feez2"

-- foo3 : Foo2
-- foo3 = Feez2 5

-- foo4 : Foo2
-- foo4 = Feez2 NoS1

-- test3 : String
-- test3 = dshow2 foo3

-- test4 : String
-- test4 = dshow2 foo4

