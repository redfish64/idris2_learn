
--(b,(c,(d,a)))
test1 : String
test1 = foldr (\y,x => ("(" ++ y ++ "," ++ x ++ ")" )) "a" ["b","c","d"]

--(((a,b),c),d)
test2 : String
test2 = foldl (\x,y => ("(" ++ x ++ "," ++ y ++ ")" )) "a" ["b","c","d"]
