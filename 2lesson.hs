sum' [] = 0
sum' (x:xs) = x + sum' xs 

length'[] = 0
length' (x:xs) = 1 + length' xs

inverse' [] = []
inverse' (x:xs) = (inverse' xs) ++ [x]

concat' [] y = y
concat' x y =  concat' (init x) (last x:y)

