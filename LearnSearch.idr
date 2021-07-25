
data Elem : (v : a) -> (vs : List a) -> Type where
--search here doesn't change the outcome, maybe speed of typecheck?
     [search v]
     Here : Elem x (x :: xs)
     There : Elem x xs -> Elem x (y :: xs)


foo2 : (x : Char) -> (xs : List Char) -> {auto e : Elem x xs} -> List Char
foo2 x xs = x :: x :: x :: xs

foo : List Char -> List Char
foo xs =
  let x : Char = 'x'
      xs' : List Char = x :: xs
      xs2 : List Char = x :: x :: []
  in
    -- foo2 x xs' --neither of these work
    -- foo2 x xs2 
    []
   
--when we move everything into "where", though: 
foo' : List Char -> List Char
foo' xs =
    --both work fine
    foo2 x xs' ++ foo2 x xs2 
  where
      x : Char 
      x = 'x'
      xs' : List Char 
      xs' = x :: xs
      xs2 : List Char 
      xs2 = x :: x :: []
      -- e : Elem x xs2
      -- e = Here {x=x} {xs = x :: []}
    
data MyShow : Type -> Type where
     [noHints]
     MkMyShow : (myshow : a -> String) -> MyShow a

%hint
showBool : MyShow Bool
showBool = MkMyShow (\x => if x then "True" else "False")

-- myShow : MyShow a => a -> String
myShow : {auto _ : MyShow a} -> a -> String
myShow @{MkMyShow myshow} = myshow

testShow : String
testShow = myShow True
