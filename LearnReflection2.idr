import Language.Reflection

%language ElabReflection

powerFn : Nat -> TTImp
powerFn Z = `(const 1)
powerFn (S k) = `(\x => mult x (~(powerFn k) x))

-- Note: this example doesn't quite do what we want yet. Ideally, we'd find
-- a way to block reduction under the 'pure' while running the script
powerFn' : Nat -> Elab (Nat -> Nat)
powerFn' Z = pure (const 1)
powerFn' (S k)
    = do powerk <- powerFn' k
         pure (\x => mult (powerk x) x)

%macro
power : Nat -> Elab (Nat -> Nat)
power n = check (powerFn n)

%macro
power' : Nat -> Elab (Nat -> Nat)
power' n = powerFn' n

cube : Nat -> Nat
cube = power 3

cube' : Nat -> Nat
cube' = power' 3


record Foo where
  constructor MkFoo
  bar : Int
  bee : Char
 
foo : TTImp
foo = `(Foo)

foo2 : TTImp
foo2 = `(MkFoo 1 'c')

foo3 : Name
foo3 = `{{MkFoo}}

-- namespace A 
--   foo : TTImp
--   foo = `(record Foo where
--             constructor MkFoo
--             bar : Int
--             bee : Char)

names : List Name
names = [ `{{ names }}, `{{ Prelude.(+++@#@!) }} ]
