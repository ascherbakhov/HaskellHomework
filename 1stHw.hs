--Sherbakov ALeksandr 271
prim' n d 
	|(n==1) = False
	|(n `div` 2)  < d = True
	|(n `mod` d == 0) = False
	|otherwise = prim' n (d+1)
prim n = prim' n 2

gcd' n m 
	| (m > n) = gcd' m n
	| ((n `mod` m) == 0) = m
	| otherwise = gcd' m (n `mod` m)
		  
rprim n m = (gcd' n m == 1)

lcm' a b = (a*b) `div` (gcd' a b)

lnd' n d ch
	|(n `div` 2 < d) = ch+1
	|(n `mod` d == 0) = lnd' n (d+1) (ch+1)
	|otherwise = lnd' n (d+1) (ch)
lnd n = lnd' n 1 0

lsd' n d sum
	|((n `div` 2) < d) = sum+n
	|(n `mod` d == 0) = lsd' n (d+1) (sum+d)
	|otherwise = lsd' n (d+1) (sum)
lsd n = lsd' n 1 0

euler' n d ch
	|(n == d) = ch
	|(gcd' n d == 1) = euler' n (d+1) (ch+1)
	|otherwise = euler' n (d+1) (ch)
euler n = euler' n 1 0  