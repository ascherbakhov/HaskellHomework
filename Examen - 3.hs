data L a = N | O (L a) a (L a) |E (L a) (L a) 

tklast::L a->a
tklast list = case list of
     O N a N -> a
     O a b c -> tklast c
     E a b -> tklast b

hd:: L a->a
hd list = case list of
	O N a N -> a
	O a b c -> hd a
	E a b -> hd a

tail:: L a -> a
tail list = case list of
	O N a N ->N
	O a b c = E  


tnlast::L a->L a
tnlast list = case list of
	O N a N -> N
	E a b -> O (tnlast a) (tklast a) (tnlast b)
	O a b c -> E a (cons b (tnlast c)) 


cons:: a -> L a-> L a
cons p list  =  case list of 
    N -> O N p N
    O a b c -> E (cons p a) (cons b c)
    E a b -> O (cons p (tnlast a)) (tklast a) b


fromList:: [a] -> L a 
fromList list = case list of
	x:xs -> cons x (fromList xs)
	[] -> N

toList::L a -> [a]
toList list = case list of 
	O N p N -> [p]
	E a b -> (toList a) ++ (toList b) 
	O a b c ->( toList a) ++ [b] ++ (toList c)

mapL::(a->b) -> L a -> L b
mapL f list = case list of
	O a b c -> O (mapL f a) (f b) (mapL f c)
	E a b -> E (mapL f a) (mapL f b)
	N -> N


revert:: L a -> L a
revert list = case list of
	O a b c -> O (revert c) b (revert a)
	E a b -> E (revert b) (revert a)
	N -> N


foldL:: (a -> b -> b) -> L a -> b -> b
foldL f list acc = case list of
	E a b -> foldL f b $ foldL f a acc
	O a b c -> foldL f c $ f b $ foldL f a acc 
	N -> acc


len:: L a -> Integer
len list = case list of
	E a b -> 2 * (len a)
	O a b c -> 2 * (len a) + 1
	N -> 0

concat':: L a-> L a -> L a
concat' list N = list
concat' list (O a b c) = concat (concat list a) (cons b c)
concat' list (E a b) = concat (concat list a) b

instance Show a => Show (L a) where
	show e = "[" ++ show' e ++ "]" where
		show' (E a b) = show' a ++ "," ++ show' b
		show' (O N a N) = show a
		show' (O a b c) = show' a ++ "," ++ show b ++","++show' c
		show' N = ""