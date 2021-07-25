namespace Foo
  xxx : Int -> String
  xxx _ = "42"

namespace Foo2
  xxx : String -> Int
  xxx _ = 33

-- co: typechecker can't figure out which one to use
-- yyy : Int
-- yyy = xxx "fee"

