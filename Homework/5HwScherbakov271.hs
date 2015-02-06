data Lambda = Var String|
			  App Lambda Lambda|
			  Lam String Lambda
	deriving(Show)

show' (Var x) = x
show' (Lam x y) = "\\" ++ x ++ show' y
show' (App x y) = (lam_x $ show' x) ++ (app_y $ show' y) where
	lam_x = case x of Lam _ _ ->br; _ ->id
	app_y = case y of App _ _ -> br; _ -> id
	br x = "(" ++ x ++ ")"

fv (Var x) = [x]
fv (App x y) = (fv x) ++ (fv y)
fv (Lam x a) = [ y | y <- fv(a), y /= x ]

subst (Var x) (Var x') b | x==x' = b
subst (Var y) (Var x) b = Var y
subst (App m n) (Var x) b = App (subst m (Var x) b) (subst n (Var x) b)
subst (Lam x a) (Var y) b | (y==x) = Lam x a
subst (Lam x a) (Var y) b = Lam y (subst a (Var x) b)

subst' (Var x) (Var x') b | x==x' = b
subst' (Var y) (Var x) b = Var y
subst' (App m n) (Var x) b = App (subst m (Var x) b) (subst n (Var x) b)
subst' (Lam x a) (Var y) b | (y==x) = Lam x a
subst' (Lam y a) (Var x) b | not $ elem y (fv b) = Lam y (subst' a (Var x) b)
subst' (Lam y a) (Var x) b | elem y (fv b) = subst' (Lam z (subst' a (Var y) (Var z))) (Var x) b
	where z = [head $ filter (\x -> not (elem [x] (fv a))  && not(elem [x] (fv b))) ['a'..]]
	
reduce (Var x) = Var x
reduce (Lam x a) = Lam x $ (reduce a)
reduce (App (Lam x a) b) = reduce $ subst' a (Var x) b
reduce (App a b) =
		let a'' = reduce a in case (a'') of
			(Lam x a') -> reduce $ subst' a' (Var x) b
			_ -> App (a'') (reduce b)
		
		