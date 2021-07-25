
data LinList : (vt : Type) -> Type where
  LNil : LinList vt
  LCons : (1 v : vt) -> LinList vt -> LinList vt
  
makeUniqueSet : List v -> (1 f : (1 _ : v) -> LinList v -> LinList v) -> List v
makeUniqueSet xs f = ?makeUniqueSet_rhs


-- data LinDAG : (vt : Type) -> Type where
--   LDStart : (v : vt) -> LinDAG vt
--   LDOneToMany : (from : LinList (LinDAG vt)) -> List (LinDAG vt) -> LinDAG vt
--   LDManyToOne : (from : LinList (LinDAG vt)) -> List (LinDAG vt) -> LinDAG vt
 
-- test1 : ()
-- test1 = let l1x = LDStart 5
--             l2x = LDNode 42
--             l2x = LDEdge x x1
--             x3 = LDEdge x1 x
--             x4 = 
            
--         in ?xx
 
-- addToDAG : (List vt) -> (
