getelementofsize :: Int -> [[x]] -> [x]
getelementofsize _ [] = []
getelementofsize num (x:xs) =   if num == length x
				then x
				else getelementofsize num xs

shortest :: [[a]] -> [a]
shortest x = 			let minlist = minimum (map (length) x)
		          	in getelementofsize minlist x


