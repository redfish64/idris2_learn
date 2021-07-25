import Data.Vect


--both impl's same same. We can use 'm' or just declare forall n. In either case we are creating a new local variable. If we just try to use 'n' then n is already bound in outer context.
Show a => Show (Vect n a) where
    show xs = "[" ++ show' xs ++ "]" where
        show' : Vect m a -> String
        show' Nil        = ""
        show' (x :: Nil) = show x
        show' (x :: xs)  = show x ++ ", " ++ show' xs
        
-- Show a => Show (Vect n a) where
--     show xs = "[" ++ show' xs ++ "]" where
--         show' : forall n . Vect n a -> String
--         show' Nil        = ""
--         show' (x :: Nil) = show x
--         show' (x :: xs)  = show x ++ ", " ++ show' xs

-- won't work
-- Show a => Show (Vect n a) where
--     show xs = "[" ++ show' xs ++ "]" where
--         show' : Vect n a -> String
--         show' Nil        = ""
--         show' (x :: Nil) = show x
--         show' (x :: xs)  = show x ++ ", " ++ show' xs
        
