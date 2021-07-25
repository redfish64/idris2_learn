module LearnLinearity5a

import LearnLinearity5

-- co: doesn't compile since getSecret is "0" count (which is pretty cool)
-- test1 : Int
-- test1 = getSecret (mkSecret True)

test2 : getSecret (mkSecret True) = 42
test2 = Refl

