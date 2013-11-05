--	My HASKELL list functions. Lab 4.

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) y = x:myAppend xs y

myHead :: [a] -> a
myHead (x:xs) = x

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast(xs)

myTail :: [a] -> [a]
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = myAppend [x] (myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = myAppend x (myConcat xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs)
			| x > maxTail = x
			| otherwise = maxTail
			where maxTail = myMaximum xs
			
myMinimum :: Ord a => [a] -> a
myMinimum [x] = x
myMinimum (x:xs)
			| x < minTail = x
			| otherwise = minTail
			where minTail = myMinimum xs
			
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a [x] = a == x
myElem a (x:xs)
			| a == x = a == x
			| otherwise = myElem a xs
			
myDelete :: Eq a => a -> [a] -> [a]
myDelete a [] = []
myDelete a (x:xs)
			| a == x = xs
			| otherwise = x:myDelete a xs

-- Answers to Q2.

myUnion :: Eq a => [a] -> [a] -> [a] 

myUnion a [] = a

myUnion (x:xs) (y:ys)
			| myElem x (y:ys) = myUnion (x:xs) (myDelete x (y:ys))
			| myElem y (ys) = myUnion (x:xs) (myDelete y (y:ys))
			| otherwise = [x] ++ myUnion ((xs) ++ [y]) ys
			
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] y = []
myIntersect (x:xs) (y:ys)
			| myElem x (y:ys) = myAppend [x] (myIntersect xs (y:ys))
			| otherwise = myIntersect xs (y:ys)
