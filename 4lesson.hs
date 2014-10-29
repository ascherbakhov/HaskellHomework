data Tsil a = Lin | Snoc (Tsil a) a
	deriving(Show)
	
fold' f  a Lin = a 
fold' f a (Snoc xs x) = ((fold' f a xs) `f` x) 

addEl :: Tsil a -> a -> Tsil a
addEl list b = Snoc list b

addFs :: Tsil a -> a ->Tsil a
addFs Lin b = Snoc Lin b
addFs (Snoc xs x) b = addEl (addFs xs b) x

toList :: Tsil a -> [a]
toList (Snoc xs x) = (toList xs)++[x]
toList Lin = [] 

fromList :: [a] -> Tsil a
fromList list = case list of
	[] -> Lin
	otherwise -> addFs (fromList (tail list)) (head list)
	
length' list = fold' (\acc _ ->acc+1) 0 list

map' :: (a -> b) -> Tsil a -> Tsil b
map' func list = case list of
	Lin -> Lin
	Snoc xs x -> Snoc (map' func xs) (func x)

reverse' :: Tsil a-> Tsil a
reverse' (Snoc xs x) = addFs (reverse' xs) x
reverse' Lin = Lin 

concat' :: Tsil a -> Tsil a -> Tsil a
concat' list Lin = list
concat' list (Snoc xs x) = Snoc (list `concat'` xs) x 

flatten' :: Tsil(Tsil a) -> Tsil a	
flatten' list = fold' concat' Lin list
--merger sort
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs

mergeSort [x] = [x]
mergeSort [] = []	
mergeSort xs
    = merge (mergeSort xs1) (mergeSort xs2)
    where
    (xs1, xs2) = splitAt (length xs `div` 2) xs
