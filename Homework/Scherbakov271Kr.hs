sprod x y = sum(zipWith (*) x y)

normalize[] = []
normalize (x:xs) = if (x `elem` xs) then normalize xs else x : (normalize xs)

sort [] = []
sort (x:xs) = sort [a | a <- xs, a < x ] ++ [x] ++ sort [a | a <- xs, a >= x]

isProgression [] = True
isProgression (x:xs) = and [x ==  y| (x,y) <- (x:xs) `zip` r] where r = [x, (head xs)..]

isFunction list = (r == normalize r) where r = fst (unzip list)

isSymmetric [x] = True
isSymmetric (x:xs)  = if (snd x, fst x) `elem` xs then isSymmetric xs else False 

isReflexive list = and [(x,x) `elem` list | (x,_)<-list]
isReflexive' = (<0). length
      		  
closure r
	|r == othClos = r
	|otherwise = closure othClos
    where othClos = 
          normalize $ r ++ [(x, z) | (x, y1) <- r, (y2, z) <- r, y1 == y2]

isTransitive list = (sort $ closure list) == sort list 