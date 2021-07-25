
--Named Implementations (docs/interfaces.rst)

interface Foo x where
  fee : x -> Int
  
[foo] Foo String where
  fee = cast . length

--co: can we use a named interface by default if there is no alternative? no
-- test1 : String -> Int
-- test1 x = fee x

--must say we are using "foo" impl 
test2 : String -> Int
test2 x = fee @{foo} x

--another way of doing the above. 
interface Foo2 x where
  constructor MkFoo2
  fee2 : x -> Int

foo2 : Foo2 String 
foo2 = MkFoo2 $ cast . length

-- note that:
-- Main.fee2 : Foo2 x => x -> Int
-- So the @{} must be saying I am supplying the first `auto' arg
-- also note that there isn't a hint for foo2.
test3 : String -> Int
test3 x = fee2 @{foo2} x

--so can we name an implementation and also make it used by default?
interface Foo3 x where
  constructor MkFoo3
  fee3 : x -> Int

%hint
foo3 : Foo3 String 
foo3 = MkFoo3 $ cast . length

--yes
test5 : String -> Int
test5 x = fee3 x

--can also be named explicitly
test6 : String -> Int
test6 x = fee3 @{foo3} x

--what if we name two as "hints" for use as default?
interface Foo5 x where
  constructor MkFoo5
  fee5 : x -> Int

%hint
foo5a : Foo5 String 
foo5a = MkFoo5 $ cast . length

%hint
foo5b : Foo5 String 
foo5b = MkFoo5 $ (+1) . cast . length

--co: Now foo5a and foo5b are both valid implementations, so won't typecheck due to
--ambiguity
-- test7 : String -> Int
-- test7 x = fee5 x

--can we add hints using the syntatic sugary way of doing things?
test8 : String -> Int
test8 x = fee5 @{foo5b} x


interface Foo6 x where
  fee6 : x -> Int

-- co: no, doesn't typecheck
-- %hint  
[foo6] Foo6 String where
  fee6 = cast . length
  
