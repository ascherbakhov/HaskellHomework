--Sherbakov ALeksandr 271
prim n = prim' 2 where
	prim'  d = d*d>n || n `mod` d /= 0 && prim' (d+1)

gcd' a b = (\r -> if r==0 then b else gcd' b r) $ mod a b
		  
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

lsd n = lsd' 2 where
	lsd' d = if (n `mod` d == 0) then d + lsd' (d+1) else lsd'(d+1)

euler' n d ch
	|(n == d) = ch
	|(gcd' n d == 1) = euler' n (d+1) (ch+1)
	|otherwise = euler' n (d+1) (ch)
euler n = euler' n 1 0  