data MyShow : Type -> Type where
     [noHints]
     MkMyShow : (myshow : a -> String) -> MyShow a

%hint
showBool : MyShow Bool
showBool = MkMyShow (\x => if x then "True" else "False")

myShow : MyShow a => a -> String
myShow @{MkMyShow myshow} item = myshow item
