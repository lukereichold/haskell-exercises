-- Haskell exercises for custom List operations
-- Luke Reichold, 04/2014

-- Take a list of integers & returns a list in which each element is the sum
-- of the first and corresponding elements of the list.
addFirst :: Num a => [a] -> [a]
addFirst xs = map (+ head xs) xs


-- Take list of strings, return single string containing given strings separated by ", "
commaSeparate :: [String] -> String
commaSeparate [] = ""
commaSeparate (x:[]) = x
commaSeparate (x:xs) = x ++ ", " ++ commaSeparate xs


-- Takes an Eq item and a list, and returns a list without any occurences of the item.
-- v1) Uses list comprehension
aDeleteAll :: (Eq a) => a -> ([a] -> [a])
aDeleteAll a xs = [x | x <- xs, x /= a]

-- v2) Uses recursion
bDeleteAll :: (Eq a) => a -> ([a] -> [a])
bDeleteAll _ [] = []
bDeleteAll a (x:xs) | a == x 	= bDeleteAll a xs
					| otherwise	= x : bDeleteAll a xs


-- Takes an item and a list; returns list w/ 2nd occurrence of item removed.
deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (x:xs) = if x == a then xs else x : deleteFirst a xs

deleteSecond :: (Eq a) => a -> ([a] -> [a])
deleteSecond _ [] = []
deleteSecond a (x:xs) = if x == a then x : deleteFirst a xs else x : deleteSecond a xs


-- Input: some element and a list of tuples.
-- Output: list of the second elements of each tuple (IF input element is equal to that tuple's first element).
-- Ex: associated 3 [(3,4), (5,7), (3,6), (9,3)] = [4, 6]
associated :: (Eq a) => a -> [(a,b)] -> [b]
associated a [] = []
associated a (x:[]) = if fst x == a then [snd x] else []
associated a (x:xs) = if fst x == a then snd x : (associated a xs) else (associated a xs)
