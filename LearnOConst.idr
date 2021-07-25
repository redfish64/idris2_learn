module LearnOConst

import Control.Lens

||| implementations of `OConst m r` where it has trouble with `m . Const r`
public export
record OConst (m : Type -> Type) r v where
  constructor OConst_
  mv : m (Const r v)

public export
Functor m => Functor (OConst m r) where
  map f (OConst_ mv) = OConst_ (map (\(MkConst cv) => MkConst cv) mv)

||| Applicative without pure
public export
interface Functor m => Combinable m where
  (<*>) : m (a -> b) -> m a -> m b

public export
Applicative m => Combinable (OConst m r) where
  (<*>) (OConst_ mv) (OConst_ x) = 
    -- Here we still need to combine mv and x, but since we are dealing with Const,
    -- the value can be fabricated. So we modify the function mv so it returns what we
    -- want.
    let mv' = map (const doit) mv
        r : m (Const r a) = mv' <*> x
    in OConst_ $ map doit2 r
  where 
    doit2 : (Const r p) -> (Const r q) 
    doit2 (MkConst rv) = MkConst rv
    doit : (Const r a) -> (Const r a) 
    doit x = x

(>>) : Combinable m => m a -> m b -> m b
ma >> mb = (map (const id) ma) <*> mb

test : Applicative m => OConst m Int Int -> OConst m Int Int
test start = 
  do
    start 
    start
    
