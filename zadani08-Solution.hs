eval :: (Num a) => Expr a -> [(String, a)] -> Maybe a
eval (Var x) y = lookup x y
eval (Con x) y = Just x
eval (Add x y) z = evalCalc (eval x z) (eval y z) (+)
eval (Mul x y) z = evalCalc (eval x z) (eval y z) (*)

evalCalc :: (Num a) => Maybe a -> Maybe a -> (a -> a -> a) -> Maybe a
evalCalc (Just x) (Just y) z = Just (z x y)
evalCalc _ _ _ = Nothing


simplify01 :: Expr Integer -> Expr Integer
simplify01 (Add (Con 0) x) = simplify01 x
simplify01 (Add x (Con 0)) = simplify01 x
simplify01 (Mul (Con 1) x) = simplify01 x
simplify01 (Mul x (Con 1)) = simplify01 x
simplify01 (Mul x (Con 0)) = Con 0 
simplify01 (Mul (Con 0) x) = Con 0
simplify01 (Add x y) = simplifyFinalizer $Add (simplify01 x) (simplify01 y)
simplify01 (Mul x y) = simplifyFinalizer $Mul (simplify01 x) (simplify01 y)
simplify01 x = x

simplifyFinalizer :: Expr Integer -> Expr Integer
simplifyFinalizer (Add (Con 0) x) = simplify01 x
simplifyFinalizer (Add x (Con 0)) = simplify01 x
simplifyFinalizer (Mul (Con 1) x) = simplify01 x
simplifyFinalizer (Mul x (Con 1)) = simplify01 x
simplifyFinalizer (Mul x (Con 0)) = Con 0 
simplifyFinalizer (Mul (Con 0) x) = Con 0
simplifyFinalizer x = x


simplifyConstants :: Expr Integer -> Expr Integer
simplifyConstants (Add (Con x) (Con y)) = (Con (x + y))
simplifyConstants (Mul (Con x) (Con y)) = (Con (x * y))
simplifyConstants (Add (Con x) (Var y)) = (Add (Con x) (Var y))
simplifyConstants (Mul (Con x) (Var y)) = (Mul (Con x) (Var y))
simplifyConstants (Add (Var x) (Con y)) = (Add (Var x) (Con y))
simplifyConstants (Mul (Var x) (Con y)) = (Mul (Var x) (Con y))
simplifyConstants (Add (Var x) (Var y)) = (Add (Var x) (Var y))
simplifyConstants (Mul (Var x) (Var y)) = (Mul (Var x) (Var y))
simplifyConstants (Add x y) = simplifyFinalizer $Add (simplifyConstants x) (simplifyConstants y)
simplifyConstants (Mul x y) = simplifyFinalizer $Mul (simplifyConstants x) (simplifyConstants y)
simplifyConstants x = x

simplifyFinalizer :: Expr Integer -> Expr Integer
simplifyFinalizer (Add (Con x) (Con y)) = (Con (x + y))
simplifyFinalizer (Mul (Con x) (Con y)) = (Con (x * y))
simplifyFinalizer x = x
