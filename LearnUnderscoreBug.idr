data Foo : Int -> a -> Type where
  MkInitFoo : (v : a) -> Foo 0 a
  MkFoo : (ck : Int) -> (p : Foo pk _ ) -> (v : a) -> Foo ck a
  MkFoo' : (ck : Int) -> (p : Foo pk whyNeedMe ) -> (v : a) -> Foo ck a
  MkFoo'' : (p : Foo pk _ ) -> (ck : Int) -> (v : a) -> Foo ck a
