module LearnLens3

import Data.SortedMap

import Control.Lens
import Control.Lens.At
import Control.Monad.Identity

myMap : SortedMap Int String
myMap = SortedMap.fromList [(1,"world")] 

test1 : Maybe String
test1 = myMap ^. at 1

test2 : SortedMap Int String
test2 = set (at 2) (Just "fee") myMap

data Foo = MkFoo Int Int

-- fooa :: Lens' (Foo a) Int
fooa : Functor f => (Morphism Int $ f Int) -> (Morphism Foo $ f Foo)
fooa (Mor f) = Mor (\(MkFoo a b) => map (\a' => MkFoo a' b) $ f a)

test3 : Foo
test3 = set fooa 5 (MkFoo 1 2)

test4 : List Int
test4 = over mapped (+1) [1,2,3]

test5 : List Int
test5 = mapped &~ (+1) $ [1,2,3]

test6 : List Int
test6 = over (ix 1) (+1) [1,2,3]

test7 : List (List Char)
test7 = over (ix 1 . ix 2) toUpper $ map unpack ["foo","fee","fir"]

