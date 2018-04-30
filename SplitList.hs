oddEvenSplit :: [a] -> ([a],[a])

oddEvenSplit [] = ([],[])
oddEvenSplit xs = oddEven xs ([],[])

oddEven :: [a]->([a],[a])->([a],[a])
oddEven [] (ys,zs) = (reverse(ys),reverse(zs))
oddEven (x:[]) (ys,zs) = (reverse(x:ys),reverse(zs))
oddEven (y:z:xs) (ys,zs) = oddEven xs (y:ys,z:zs)

--Splits list to two lists with items from odd and even positions
