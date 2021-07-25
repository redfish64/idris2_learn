module LearnLens5

import Control.Lens
import Control.Monad.Identity

public export 
record CounterMonadT (0 m : Type -> Type) (a : Type) where
  constructor CMT
  runCMT : Int -> m (Int, a)

Functor f => Functor (CounterMonadT f) where
  map f (CMT runCMT) = CMT (\ctr => map (\(ctr,v) => (ctr,f v)) $ runCMT (ctr))
                               -- map (map f) $ runcmt (ctr)) --co : same as above, since (,) is a functor
                               
{r : Type} -> Functor m => Functor (m . (Const r)) where
  map f ma = map (\(MkConst rv) => MkConst rv) ma

Monad m => Applicative (CounterMonadT m) where
  pure x = CMT (\st => pure (st, x))
  
  (CMT f) <*> (CMT a)
      = CMT (\ctr =>
              do (ctr, g) <- f (ctr)
                 (ctr, b) <- a (ctr)
                 pure (ctr, g b))
 
Monad m => Monad (CounterMonadT m) where
  (CMT f) >>= k
   = CMT (\ctr =>
           do (ctr, v) <- f ctr
              let CMT kv = k v
              kv ctr)
 
interface LensAccess (m : Type -> Type) where
  lensAccess : m x -> m x
  -- totalLensAccesses : m Int
  
LensAccess (CounterMonadT m) where
  lensAccess (CMT e) = CMT (\ctr => e (ctr+1))

{m : Type -> Type} -> LensAccess m => LensAccess (m . (Const r)) where
  lensAccess e = lensAccess e --I don't know why, but this doesn't go into an inf loop

-- LensAccess (CounterMonadT m . (Const r)) where
--   lensAccess (CMT runCMT) = CMT (\ctr => runCMT (ctr+1))

data Foo = Fee Int

fooLens : {m : Type -> Type} -> Functor m => LensAccess m => Morphism Int (m Int) -> Morphism Foo (m Foo)
-- fooLens : {m : Type -> Type} -> Functor m => LensAccess m => LensLike' m Foo Int
fooLens (Mor applyMor) = 
  Mor (\(Fee v) => map Fee $ lensAccess $ applyMor v)

data Bar = Bee Foo

barLens : {m : Type -> Type} -> Functor m => LensAccess m => LensLike' m Bar Foo
barLens (Mor applyMor) = 
  Mor (\(Bee v) => map Bee $ lensAccess $ applyMor v)

MGetting : (m : Type -> Type) -> Functor m => Type -> Type -> Type -> Type
MGetting m r = LensLike' (m . (Const r))

mview : Monad m => MGetting m a s a -> s -> m a
mview l = 
  let afunc = (\a => pure $ MkConst a)
      (Mor sfunc) = l (Mor afunc)
      resFunc = map (\(MkConst a) => a) . sfunc
  in resFunc
 
-- type ASetter s t a b = (a -> Identity b) -> s -> Identity t
public export
MSetter : (m : Type -> Type) -> Type -> Type -> Type -> Type -> Type
MSetter = LensLike

mset : Monad m => MSetter m s s a a -> a -> s -> m s
mset l v s = 
  let (Mor sf) = l $ Mor (\a => pure v)
  in sf s
 
test : CounterMonadT Identity Bar
test =
  do
    let f = Fee 5
    let b = Bee f
    g <- mview (fooLens {m=CounterMonadT Identity . (Const Int)}) f
    f2 <- mset fooLens 12 f
    -- g <- mview {m=CounterMonadT Identity} {a=Int} (fooLens ) f2
    -- g <- the (CounterMonadT Identity Int) $ mview {m=CounterMonadT Identity} {a=Int} {s=Foo} 
    --         (the (Morphism Int (CounterMonadT Identity (Const Int Int)) -> Morphism Foo (CounterMonadT Identity (Const Int Foo))) ?ifooLens) f2
    b <- mset (barLens . fooLens) 9 b
    n <- mview (barLens {m=CounterMonadT Identity . (Const Int)} . fooLens {m=CounterMonadT Identity . (Const Int)} ) b
    pure b

-- test2 : Morphism Int ((CounterMonadT Identity . (Const Int)) Int) -> Morphism Foo (CounterMonadT Identity . (Const Int) $ Foo)
-- test2 = fooLens

runtest : (Int,Bar)
runtest =
  runIdentity $ test.runCMT 0
