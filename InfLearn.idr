module InfLearn

%default total

--won't eval under Inf so allows infinite structures
data MyStream : Type -> Type where
  (::) : a -> Inf (MyStream a) -> MyStream a

foo : MyStream Char
foo = 'a' :: foo

--without Inf, can't make this total
data MyBadStream : Type -> Type where
  (::!) : a -> MyBadStream a -> MyBadStream a

-- won't terminate
-- foo2 : MyBadStream Char
-- foo2 = (::!) 'a' foo2


