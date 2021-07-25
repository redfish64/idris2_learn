module LearnLinearity5

data Foo = Foo_ Int

foo : (1 f : Foo) -> (bar : (1 _ : Foo) -> Foo) -> Int
foo f bar = case bar (bar f) of 
              Foo_ v => v

--not sure what the "1" here means
1 fee : Int
fee = 5

--see LearnLinearity5a for how this works
public export
record MySecret where
  constructor MySecret_
  secret : Int

public export
mkSecret : Bool -> MySecret
mkSecret True = MySecret_ 42
mkSecret False = MySecret_ 7


public export
0 getSecret : MySecret -> Int
getSecret = secret 
