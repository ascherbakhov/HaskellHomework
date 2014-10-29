length' = foldl (\acc _ -> acc + 1) 0
minl (x:xs) = foldl min x xs
maxl (x:xs) = foldl max x xs

primes = [n| n<-[3,5..], foldr (&&) True [n `mod` del /= 0| del<-[2 .. n `div` 2]]] 
fibs = 1 : 1 : [x + y | (x,y) <- fibs `zip` (tail fibs)]
