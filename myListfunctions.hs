--	My HASKELL list functions. Lab 4.
--  Answers to Q1.

myAppend :: [a] -> [a] -> [a]
myAppend x y = x ++ y

myHead :: [a] -> a
myHead (x:xs) = x

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast(xs)

myTail :: [a] -> [a]
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = [x] ++ myInit xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

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
myElem a [x] = a == x
myElem a (x:xs)
			| a == x = a == x
			| otherwise = myElem a xs
			
myDelete :: Eq a => a -> [a] -> [a]
myDelete a [] = []
myDelete a (x:xs)
			| a == x = xs
			| otherwise = x:myDelete a xs
