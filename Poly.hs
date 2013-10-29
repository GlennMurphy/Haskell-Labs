type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly
sumPoly first [] = first
sumPoly [] second = second
sumPoly (first:ftail) (second:stail) = [first + second] ++ sumPoly ftail stail
