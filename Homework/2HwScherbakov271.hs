		rnorm (c1, z1) = (c1 `div` r, z1 `div` r) where r = (gcd c1 z1)
radd (c1, z1) (c2, z2) = rnorm(c1*z2+c2*z1, z1*z2)
rsub (c1, z1) (c2, z2) = rnorm(c1*z2+c2*z1, z1*z2)
rmul (c1, z1) (c2, z2) = rnorm(c1*c2, z1*z2)
rinv (c1, z1) = (z1, c1)

map' list fun = [fun x|x<-list]

inverse' (x:xs) = inverse'' [] (x:xs) where	 	
	inverse'' acc (x:xs) = inverse'' (x:acc) xs 
	inverse'' acc [] = acc

zip' (x:xs) (y:ys) = (x, y): (zip' xs ys)
zip' _ _ = []

flatten' [] = []
flatten' (x:xs) = [xn| xn<-x] ++ (flatten' xs)

unzip' [] = ([],[])
unzip' ((x,y):xs) = (x:tx, y:ty) where
	(tx, ty) = unzip' xs
	


