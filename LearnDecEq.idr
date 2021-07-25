import Decidable.Equality

data Flippy = Dolphin | Shark

DecEq Flippy where
  decEq Dolphin Dolphin = Yes Refl
  decEq a@(Dolphin) b@(Shark) = No foo
    where foo : (Dolphin = Shark) -> Void
        --foo Refl impossible
          foo Refl impossible
  -- decEq Dolphin Shark = No (\x => case x of Refl impossible)
  decEq Shark Dolphin = No (\x => case x of Refl impossible)
  decEq Shark Shark = Yes Refl


-- - + Main.xxx [P]
--  `--         b : Flippy
--             b' : Flippy
--           prf2 : b = b'
--              a : Flippy
--             a' : Flippy
--            prf : a = a'
--           fPrf : MkAqua a b = MkAqua a b
--          fPrf2 : MkAqua a b = MkAqua a' b
--          fPrf3 : MkAqua a b = MkAqua a' b'
--      --------------------------------------------
--       Main.xxx : Dec (MkAqua a b = MkAqua a' b')

-- DecEq Aqua where
--   -- decEq : (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
--   decEq (MkAqua a b) (MkAqua a' b') with (decEq a a') 
--     decEq (MkAqua a b) (MkAqua a' b') | (No contra) =
--        No $ contra . aquaEqFirstArgEq
--     decEq (MkAqua a b) (MkAqua a' b') | (Yes prf) with (decEq b b')
--     decEq (MkAqua a b) (MkAqua a' b') | (Yes prf) | (No contra) =
--        No $ contra . aquaEqSndArgEq
--     decEq (MkAqua a b) (MkAqua a' b') | (Yes prf) | (Yes prf2) = 
--         Yes (rewrite (sym prf) in (rewrite sym prf2 in Refl))
        -- let fPrf = the (MkAqua a b = MkAqua a b) Refl
        --     fPrf2 = the (MkAqua a b = MkAqua a' b) (rewrite (sym prf) in fPrf)
        --     fPrf3 = the (MkAqua a b = MkAqua a' b') (rewrite (sym prf2) in fPrf2)
        -- in
        --   Yes fPrf3
    
data Aqua = MkAqua Flippy Flippy

DecEq Aqua where
  -- decEq : (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
  decEq (MkAqua a b) (MkAqua a' b') = 
    let 
      aquaEqFirstArgEq : {a,b,c,d : _ } -> (MkAqua a b) === (MkAqua c d) -> a = c
      aquaEqFirstArgEq Refl = Refl
      aquaEqSndArgEq : (MkAqua a b) = (MkAqua c d) -> b = d
      aquaEqSndArgEq Refl = Refl
    in 
      case (decEq a a') of 
         (No contra) => No $ contra . aquaEqFirstArgEq
         (Yes prf) => case (decEq b b') of 
                        (No contra) => No $ contra . aquaEqSndArgEq
                        (Yes prf2) => Yes (rewrite (sym prf2) in (rewrite (sym prf) in Refl))
                        
                        
                                    -- let fPrf = the (MkAqua a b = MkAqua a b) Refl
                                    --     fPrf2 = the (MkAqua a b = MkAqua a' b) (rewrite (sym prf) in fPrf)
                                    --     fPrf3 = the (MkAqua a b = MkAqua a' b') (rewrite (sym prf2) in fPrf2)
                                    -- in
                                    --   Yes fPrf3
  
  
 
data Aqua2 = 
 MkAqua2 Flippy Flippy | EmptyAqua2

-- public export
-- decEqAqua2 : (x : Aqua2) -> (y : Aqua2) -> Dec ( x = y)  
-- decEqAqua2 (MkAqua2 a b) (MkAqua2 a' b') = 
--   let 
--     aquaEqSndArgEq : (MkAqua2 a b) = (MkAqua2 c d) -> b = d
--     aquaEqSndArgEq Refl = Refl
--     aquaEqFirstArgEq : {a2,b2,c2,d2 : Flippy} -> (MkAqua2 a2 b2) = (MkAqua2 c2 d2) -> a2 = c2
--     aquaEqFirstArgEq Refl = Refl
--   in 
--     case (decEq a a') of 
--        (No contra) => No $ contra . aquaEqFirstArgEq
--        (Yes prf) => case (decEq b b') of 
--                       (No contra) => No $ contra . aquaEqSndArgEq
--                       (Yes prf2) => Yes (rewrite (sym prf) in (rewrite sym prf2 in Refl))
-- decEqAqua2 EmptyAqua2 EmptyAqua2 = Yes Refl
-- decEqAqua2 a@(MkAqua2 x z) b@EmptyAqua2 = 
--   let foo : (a = b) -> Void
--       foo Refl impossible
--   in No foo
-- decEqAqua2 a@EmptyAqua2 b@(MkAqua2 x y) = 
--   let foo : (a = b) -> Void
--       foo Refl impossible
--   in No foo

-- DecEq Aqua2 where
--   decEq = decEqAqua2
  
-- data Aqua3 : Flippy -> Type where
--   MkAqua3 : (a : Flippy) -> Aqua3 a
  
-- public export
-- decEqAqua3 : (x : Aqua3 a) -> (y : Aqua3 a) -> Dec ( x = y)  
-- decEqAqua3 (MkAqua3 _) (MkAqua3 _) = Yes Refl

-- data Aqua5 : Type where
--   MkAqua5 : (0 a : Flippy) -> Aqua5
  
-- -- public export
-- -- decEqAqua5 : (x : Aqua5) -> (y : Aqua5) -> Dec ( x = y)  
-- -- decEqAqua5 (MkAqua5 a) (MkAqua5 b) with (a = b)
-- --    decEqAqua5 (MkAqua5 a) (MkAqua5 b) | Yes Refl = ?xxx
-- --    decEqAqua5 (MkAqua5 a) (MkAqua5 b) | No ?xxx2 = ?xxxy
 


  
-- -- public export
-- -- decEqAqua3 : (x : Aqua5 a) -> (y : Aqua5 a) -> Dec ( x = y)  
-- -- decEqAqua3 (MkAqua3 _) (MkAqua3 _) = Yes Refl
public export 
data Currency = AUD | USD | CAD -- none is a neutral amount, with no currency, such as a multiplication
                                -- factor
myDecEq : (x : Currency) -> (y : Currency) -> Dec (x = y) --Either (x = y) (x = y -> Void)


DecEq Currency where
  decEq = myDecEq
