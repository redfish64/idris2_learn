import Data.Strings
import Data.List

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (stringToNatOrZ input))
     else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers
  = do Just x_ok <- readNumber
            | Nothing => pure Nothing
       Just y_ok <- readNumber
            | Nothing => pure Nothing
       pure (Just (x_ok, y_ok))
       
main : IO ()
main = 
  do 
    Just (x,y) <- readNumbers 
          | Nothing => putStrLn "your numbers, they stink!"
    putStrLn $ show x ++ " " ++ show y
  
m_add : Maybe Int -> Maybe Int -> Maybe Int
m_add x y = pure (!x + !y)

m_add' : Maybe Int -> Maybe Int -> Maybe Int
m_add' x y = do 
   x' <- x
   y' <- y
   pure $ x' + y'
   
-- m_add, m_add' same-same

   
--this is Monad comprehensions, meant to work with Alternative
-- note the qualifiers (after |) can be "guard" or "let" as well (not shown)   
m_add'' : Maybe Int -> Maybe Int -> Maybe Int
m_add'' x y = [ x' + y' | x' <- x, y' <- y ]

--this uses Idiom Brackets. It turns function application into <*> calls
m_add''' : Maybe Int -> Maybe Int -> Maybe Int
m_add''' x y = [| x + y |]

m_add'''same_same : Maybe Int -> Maybe Int -> Maybe Int
m_add'''same_same x y = (pure (+) <*> x) <*> y


data Expr = Var String      -- variables
          | Val Int         -- values
          | Add Expr Expr   -- addition
          
data Eval : Type -> Type where
     MkEval : (List (String, Int) -> Maybe a) -> Eval a 
 
fetch : String -> Eval Int
fetch x = MkEval (\e => fetchVal e) where
    fetchVal : List (String, Int) -> Maybe Int
    fetchVal [] = Nothing
    fetchVal ((v, val) :: xs) = if (x == v)
                                  then (Just val)
                                  else (fetchVal xs)
                       
Functor Eval where
    map f (MkEval g) = MkEval (\e => map f (g e))

Applicative Eval where
    pure x = MkEval (\e => Just x)

    (<*>) (MkEval f) (MkEval g) = MkEval (\x => app (f x) (g x)) where
        app : Maybe (a -> b) -> Maybe a -> Maybe b
        app (Just fx) (Just gx) = Just (fx gx)
        app _         _         = Nothing
 
eval : Expr -> Eval Int
eval (Var x)   = fetch x
eval (Val x)   = [| x |] -- calls pure 
eval (Add x y) = [| eval x + eval y |] --calls pure on plus, and <*> on its args

runEval : List (String, Int) -> Expr -> Maybe Int
runEval env e = case eval e of
    MkEval envFn => envFn env
    
--named implementation (for when you want multiple)
[myord] Ord Nat where
   compare Z (S n)     = GT
   compare (S n) Z     = LT
   compare Z Z         = EQ
   compare (S x) (S y) = compare @{myord} x y

testList : List Nat
testList = [3,4,1]

testShow : String 
testShow = show (sort testList)

--sorts using named implemation
testShow2 : String
testShow2 = show (sort @{myord} testList)

-- interface Semigroup ty where
--   (<+>) : ty -> ty -> ty

-- interface Semigroup ty => Monoid ty where
--   neutral : ty

--more named implemation shit
[PlusNatSemi] Semigroup Nat where
  (<+>) x y = x + y

[MultNatSemi] Semigroup Nat where
  (<+>) x y = x * y
  
[PlusNatMonoid] Monoid Nat using PlusNatSemi where
  neutral = 0

[MultNatMonoid] Monoid Nat using MultNatSemi where
  neutral = 1
  
-- a way to help the compiler find implemenations. in this
-- interface, only 'm' is needed to find an implementation
-- 's' is random, since it's the implementation of the state     
interface Monad m => MonadState s (m : Type -> Type) | m where
  get : m s
  put : s -> m ()
  
