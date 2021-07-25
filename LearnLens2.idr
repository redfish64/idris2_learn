module LearnLens2

import Control.Monad.Identity

(.~) : (a -> b) -> (c -> a) -> (c -> b)
-- (.~) : (d -> e) -> (f -> d) -> (f -> e)
-- (.~) : (x -> y) -> (z -> x) -> (z -> y)
(.~) f g x = f (g x)

infixr 9 .~

-- foo = (.~) .~ (.~)
-- z = d -> e
-- x = (f -> d) -> f -> e
-- x = a -> b
-- y = (c -> a) -> (c -> b)
-- a = f -> d
-- b = f -> e
-- y = (c -> f -> d) -> (c -> f -> e)
-- z -> y = (d -> e) -> (c -> f -> d) -> c -> f -> e
-- ans    = (d -> e) -> (c -> f -> d) -> c -> f -> e

foo : (a -> b) -> (c -> d -> a) -> (c -> d -> b)
foo = (.~) .~ (.~)

map_dot_map : Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
map_dot_map = map . map


Setter : Type -> Type -> Type -> Type -> Type
Setter s t a b = (a -> Identity b) -> s -> Identity t

over : Setter s t a b -> (a -> b) -> s -> t 
over set f x = runIdentity $ set (Id . f) x

mapped : Functor f => Setter (f a) (f b) a b
mapped f = Id . map (runIdentity . f)

chars : Setter String String Char Char
chars f x = map pack $ mapped f (unpack x)
--chars f x = Id $ pack $ over mapped (runIdentity . f) (unpack x)
