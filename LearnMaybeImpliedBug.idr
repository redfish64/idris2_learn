module LearnMaybeImpliedBug

data FooNat : Nat -> Type where
  MkFN : (x : Nat) -> FooNat x

foo1 : {v : Nat} -> Type
foo1 {v} = FooNat v

-- bar1 : LearnMaybeImpliedBug.foo1 @{v=5}
-- bar1 = MkFN 5

foo2 : (v : Nat) -> Type
foo2 v = FooNat v

bar2 : LearnMaybeImpliedBug.foo2 5
bar2 = MkFN 5

x : Type
x = foo1 {v=5}

bar3 : LearnMaybeImpliedBug.foo1 {v=5} -> Int
bar3 _ = 5

bar4 : LearnMaybeImpliedBug.foo2 5 -> Int
bar4 _ = 5

