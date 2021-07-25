--can funcs work nicely with interfaces? no

-- interface FuncDesc (foo : (x ** y ** x -> y)) where
--   desc : Maybe foo -> String
  
-- FuncDesc (x ** y ** (+)) where
--   desc x = "(+)"

-- FuncDesc (-) where
--   desc x = "(-)"


-- test1 : String
-- test1 = desc (+)

--interfaces can have constructors
interface Foo x where
  constructor MkFoo
  fee : x -> Int

--are they just records?  
record Foo' x where
  constructor MkFoo'
  fee' : x -> Int

--constructors are optional for records, too 
record Bar x where
  bee : x -> Int

test1 : Foo x => x -> Int
test1 x = fee x

--co: not just records, cause this doesn't work:
-- test2 : Foo' x => x -> Int
-- test2 x = fee' x

--co: nor this:
-- test2 : Bar x => x -> Int
-- test2 v = bee v

--somehow it sneaks in the @{fy} for interfaces, but for records you have to add it and use it explicitly
test5 : Foo' x => x -> Int
test5 @{fy} y = fy.fee' y

--same here, for the record without the constructor
test6 : Bar x => x -> Int
test6 @{fy} y = fy.bee y

--interfaces are pretty close to records, but have some differences:

-- λΠ> :doc Foo'
-- Main.Foo' : Type -> Type
--   Totality: total
--   Constructor: MkFoo' : (x -> Int) -> Foo' x
--   Projection: fee' : Foo' x -> x -> Int
-- λΠ> :doc Bar
-- Main.Bar : Type -> Type
--   Totality: total
--   Constructor: __mkBar : _
--   Projection: bee : Bar x -> x -> Int
-- λΠ> :doc Foo
-- Main.Foo : Type -> Type
--   Parameters: x
--   Methods:
--     fee : x -> Int
    
-- λΠ> :type __mkBar
-- Error: Undefined name mkBar. 

-- (Interactive):1:9--1:14
--  1 | :type __mkBar
--              ^^^^^
-- λΠ> :type 
-- Expected 'case', 'if', 'do', application or operator expression.

-- (Interactive):1:7--1:8
--  1 | :type 
--            ^
-- λΠ> :type "__mkBar"
-- fromString "__mkBar" : String
-- λΠ> :type `{"__mkBar"}
-- Expected 'case', 'if', 'do', application or operator expression.

-- (Interactive):1:7--1:8
--  1 | :type `{"__mkBar"}
--            ^
-- λΠ> :type {"__mkBar"}
-- Expected 'case', 'if', 'do', application or operator expression.

-- (Interactive):1:7--1:8
--  1 | :type {"__mkBar"}
--            ^
-- λΠ> :type `{__mkBar}
-- Expected 'case', 'if', 'do', application or operator expression.

-- (Interactive):1:7--1:8
--  1 | :type `{__mkBar}
--            ^

--can't seem to reference any function with _ as the first char

