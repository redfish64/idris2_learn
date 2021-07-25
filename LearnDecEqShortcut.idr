
data Instruction : Type where 
  Add : Instruction 
  Mult : Instruction 
  Div : Instruction
  
data View : Instruction -> Instruction -> Type where
  VAdd : View Add Add
  VMult : View Mult Mult
  VDiv : View Div Div
 
view : (a, b : Instruction) -> Maybe (View a b)
 
-- same as viewDiag : (a : Instruction) -> (view a a = Nothing -> Void)
viewDiag : (a : Instruction) -> Not (view a a = Nothing)
