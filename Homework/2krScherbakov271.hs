import Data.List
--1
fromFun fun =  map (\x -> (x, fun x))
--2
dom = map fst
--3
eval ((q, fq):ts) x 
	|x==q = fq
	|otherwise = eval ts x
--4
invert = map (\(x,fx)->(fx,x))
--5
infixl 9 .*.
(.*.) t1 t2 = [(a,c) | (a,b) <- t1, (b',c) <- t2, b == b']
--6
image tb ls = nub [y | x' <- ls, (x, y) <- tb, x == x']
--7
preimage tb ls = [x | y' <- ls, (x, y) <- tb, y == y']
--8
isInjective tb = injective tb [] where
	injective ((_,y):tbs) acc =  (not $ y `elem` acc) && injective tbs (y:acc)
	injective _ _ = True
--9
isSurjective = (<0).length
--10
areMutuallyInverse x y | sort x == sort(invert y) = True
areMutuallyInverse _ _ = False