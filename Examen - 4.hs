permut :: [a] -> [[a]]

pr a l = pr' l [] a
	where
		pr' [] l2 a = [l2 ++ [a]]
		pr' (x:xs) l2 a = (l2++[a]++(x:xs)) : (pr' xs (x:l2) a)

permut (x:[]) = [[x]]
permut (x:xs) = concat (map (pr x) (permut xs))