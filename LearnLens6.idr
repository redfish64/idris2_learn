module LearnLens6

import Control.Lens
import Control.Monad.Identity

public export 
record CounterMonadT (0 m : Type -> Type) (a : Type) where
  constructor CMT
  runCMT : Int -> m (Int, a)

Functor f => Functor (CounterMonadT f) where
  map f (CMT runCMT) = CMT (\ctr => map (\(ctr,v) => (ctr,f v)) $ runCMT (ctr))
                               -- map (map f) $ runcmt (ctr)) --co : same as above, since (,) is a functor
                               
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

record OConst (m : Type -> Type) r v where
  constructor OConst_
  mv : m (Const r v)

Functor m => Functor (OConst m r) where
  map f (OConst_ mv) = OConst_ (map (\(MkConst cv) => MkConst cv) mv)
  
{r : Type} -> {m : Type -> Type} -> LensAccess m => LensAccess (OConst m r) where
  lensAccess (OConst_ mv) = OConst_ (lensAccess mv)  
      
-- LensAccess (CounterMonadT m . (Const r)) where
--   lensAccess (CMT runCMT) = CMT (\ctr => runCMT (ctr+1))

data Foo = Fee Int

fooLens : {m : Type -> Type} -> Functor m => LensAccess m => LensLike' m Foo Int
fooLens (Mor applyMor) = Mor (\(Fee v) => map Fee $ lensAccess $ applyMor v)

data Bar = Bee Foo

barLens : {m : Type -> Type} -> Functor m => LensAccess m => LensLike' m Bar Foo
barLens (Mor applyMor) = 
  Mor (\(Bee v) => map Bee $ lensAccess $ applyMor v)

MGetting : (m : Type -> Type) -> Functor m => Type -> Type -> Type -> Type
MGetting m r = LensLike' (OConst m r)

mview : Monad m => MGetting m a s a -> s -> m a
mview l =
  let afunc : (a -> OConst m a a) = (\a => OConst_ $ pure (MkConst a))
      (Mor sfunc) = l (Mor afunc)
      resFunc = (\(OConst_ mca) => map (\(MkConst a) => a) mca) . sfunc
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
    g <- mview fooLens f
    f2 <- mset fooLens 12 f
    g <- mview fooLens f2
    b <- mset (barLens . fooLens) 9 b
    n <- mview (barLens . fooLens) b
    pure b

runtest : (Int,Bar)
runtest =
  runIdentity $ test.runCMT 0
