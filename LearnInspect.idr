import Decidable.Equality
-- https://agda.github.io/agda-stdlib/README.Inspect.html

------------------------------------------------------------------------
-- Using inspect

------------------------------------------------------------------------
-- Inspect


-- from https://agda.github.io/agda-stdlib/Relation.Binary.PropositionalEquality.html#2382
-- Inspect can be used when you want to pattern match on the result r
-- of some expression e, and you also need to "remember" that r ≡ e.

-- See README.Inspect for an explanation of how/why to use this.

-- record Reveal_'_is_ {A : Set a} {B : A → Set b}
--                     (f : (x : A) → B x) (x : A) (y : B x) :
--                     Set (a ⊔ b) where
--   constructor [_]
--   field eq : f x ≡ y
  
record Reveal_fx_eq_y {b : a -> Type} (f : (x : a) -> b x) (x : a) (y : b x) where
  constructor MkRI
  eq : f x = y
-- record Reveal_fx_eq_y {a,b : Type} (f : (x : a) -> b) (x : a) (y : b) where
--   constructor MkRI
--   eq : f x = y
--   fx : b

-- inspect : ∀ {A : Set a} {B : A → Set b}
--           (f : (x : A) → B x) (x : A) → Reveal f · x is f x
-- inspect f x = [ refl ]
inspect : {b : a -> Type} -> (f : (x : a) -> b x) -> (x : a) -> Reveal_fx_eq_y f x (f x)
inspect {b=b} f x = MkRI {b=b} Refl 

-- We start with the definition of a (silly) predicate: `Plus m n p` states
-- that `m + n` is equal to `p` in a rather convoluted way. Crucially, it
-- distinguishes two cases: whether `p` is 0 or not. tim: without this split, proving
-- becomes trivial
PlusEq : (m,n,p : Nat) -> Type
PlusEq m n 0 = (m = 0, n = 0)
PlusEq m n p = (m + n = p)

-- A sensible lemma to prove of this predicate is that whenever `p` is literally
-- `m + n` then `Plus m n p` holds. That is to say `∀ m n → Plus m n (m + n)`.
-- To be able to prove `Plus-eq m n (m + n)`, we need `m + n` to have either
-- the shape `zero` or `suc _` so that `Plus-eq` may reduce.

-- We could follow the way `_+_` computes by mimicking the same splitting
-- strategy, thus forcing `m + n` to reduce:
PlusEqPlus : (m,n : Nat) -> PlusEq m n (m + n)
PlusEqPlus 0 0 = (Refl, Refl)
PlusEqPlus 0 (S k) = Refl
PlusEqPlus (S k) n = Refl

-- Or we could attempt to compute `m + n` first and check whether the result
-- is `zero` or `suc p`. By using `with m + n` and naming the result `p`,
-- the goal will become `Plus-eq m n p`. We can further refine this definition
-- by distinguishing two cases like so:

-- plus-eq-with : ∀ m n → Plus-eq m n (m + n)
-- plus-eq-with m n with m + n
-- ... | zero  = {!!}
-- ... | suc p = {!!}

-- PlusEqPlus2 : (m,n : Nat) -> PlusEq m n (m + n)
-- PlusEqPlus2 m n with (m + n)
--   PlusEqPlus2 m n | 0 = ?xx
--   PlusEqPlus2 m n | S p = ?xx2

-- The problem however is that we have abolutely lost the connection between the
-- computation `m + n` and its result `p`. Which makes the two goals unprovable:

-- 1. `m ≡ 0 × n ≡ 0`, with no assumption whatsoever
-- 2. `m + n ≡ suc p`, with no assumption either

-- By using the `with` construct, we have generated an auxiliary function that
-- looks like this:
-- `plus-eq-with-aux : ∀ m n p → Plus-eq m n p`
-- when we would have wanted a more precise type of the form:
-- `plus-eq-aux : ∀ m n p → m + n ≡ p → Plus-eq m n p`.

-- This is where we can use `inspect`. By using `with f x | inspect f x`,
-- we get both a `y` which is the result of `f x` and a proof that `f x ≡ y`.
-- Splitting on the result of `m + n`, we get two cases:

-- 1. `m ≡ 0 × n ≡ 0` under the assumption that `m + n ≡ zero`
-- 2. `m + n ≡ suc p` under the assumption that `m + n ≡ suc p`

-- The first one can be discharged using lemmas from Data.Nat.Properties and
-- the second one is trivial.

-- PlusEqPlus3 : (m,n : Nat) -> PlusEq m n (m + n)
-- PlusEqPlus3 m n with (m + n)
--   PlusEqPlus3 m n | 0 with (inspect (m +) n)
--     PlusEqPlus3 m n | 0 | (MkRI fx_eq_y) = ?xx
--     PlusEqPlus3 m n | (S p) | (MkRI fx_eq_y) = ?xx2
--   PlusEqPlus3 m n | (S p) = ?xx3 --with (inspect (m +) n)
  
PlusEqPlus3 : (m,n : Nat) -> PlusEq m n (m + n)
PlusEqPlus3 m n with (m + n)
  PlusEqPlus3 m n | 0 = case (inspect (m +) n) of
                             x => ?x
  PlusEqPlus3 m n | (S p) = ?y
  
  -- (inspect (m +) n))
  -- PlusEqPlus3 m n | (MkRI Refl) = ?xxx

    
-- plus-eq-with : ∀ m n → Plus-eq m n (m + n)
-- plus-eq-with m n with m + n | inspect (m +_) n
-- ... | zero  | [ m+n≡0   ] = m+n≡0⇒m≡0 m m+n≡0 , m+n≡0⇒n≡0 m m+n≡0
-- ... | suc p | [ m+n≡1+p ] = m+n≡1+p



namespace Other
  data Instruction : Type where 
    Add : Instruction 
    Mult : Instruction 
    Div : Instruction
    AddC : Int -> Instruction
  
  maybeEq : (a, b : Instruction) -> Maybe (a = b)
  maybeEq Add Add = Just Refl
  maybeEq Mult Mult = Just Refl
  maybeEq Div Div = Just Refl
  maybeEq _ _ = Nothing

  reflNotNothing : (a : Instruction) -> Not (maybeEq a a = Nothing)
  reflNotNothing Add Refl impossible
  reflNotNothing Mult Refl impossible
  reflNotNothing Div Refl impossible

  withProof : (x : ty) -> DPair ty (\val => x=val)
  withProof x = MkDPair x Refl
  -- withProof x = (x ** Refl)

  implementation DecEq Instruction where
    -- decEq a b = case (withProof (maybeEq a b)) of
    --                  -- (MkDPair (maybeEq a b) Refl) => ?xxx --Here, we can't change Refl to y, or x will no longer unify with (maybeEq a b)
                     
    --                  --Here, we can't change y to Refl, because it states the constraint between maybeEq ?_ ?_
    --                  --and (Just Refl) can't be solved
    --                  (MkDPair (Just Refl) y) => ?xxx 
    --                  (Nothing ** maybeEqABIsNothing) => ?yyy
                     -- (Nothing ** maybeEqABIsNothing) => No (\aIsB => reflNotNothing b (replace aIsB maybeEqABIsNothing {p = \x => maybeEq x b = Nothing}))
      
    decEq a b with (withProof (maybeEq a b))
      decEq a a | ((Just Refl) ** y) = Yes Refl
      decEq a b | (Nothing ** maybeEqABIsNothing) = No (\aIsB => reflNotNothing b (replace aIsB maybeEqABIsNothing {p = \x => maybeEq x b = Nothing}))
      
