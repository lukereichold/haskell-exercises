-- Implementing 'Set' structure using Lists. 
-- Elements are sorted and non-repeating.
-- Luke Reichold, 04/2014

module Sets where 
import qualified Data.List as L
data Set a = Set [a] deriving (Show, Eq, Ord)

list2set :: Ord a => [a] -> Set a
list2set = Set . L.nub . L.sort

set2list :: Set a -> [a]
set2list (Set xs) = xs


-- Creates a single element set w/ given value
singS :: a -> Set a
singS a = Set [a]

-- Creates an empty set
emptyS :: Set a
emptyS = Set []


-- Add element to set
addToS :: (Ord a) => a -> Set a -> Set a
addToS a (Set []) = singS a
addToS a (Set xs) = Set $ add a xs
	where
		add a (x:[])
			| a < x 	= [a] ++ [x]
			| a > x 	= [x] ++ [a]
			| otherwise = [x]
		add a (x:xs)
			| a < x 	= a : (x:xs)
			| a > x 	= x : (add a xs)
			| otherwise = (x:xs)


-- Get intersection of Set A (xs) and Set B (ys)
intersectS :: (Ord a) => Set a -> Set a -> Set a
intersectS (Set xs) (Set ys) = Set $ intersect xs ys
	where
		intersect [] ys = []
		intersect xs [] = []
		intersect (x:xs) (y:ys) 
			| x < y 	= intersect xs (y:ys)
			| x > y 	= intersect (x:xs) ys
			| otherwise = x : intersect xs ys


-- Get the set-difference of Set A (xs) and Set B (ys)
diffS :: (Ord a) => Set a -> Set a -> Set a
diffS (Set xs) (Set ys) = Set $ diff xs ys
	where
		diff [] ys = []
		diff xs [] = xs
		diff (x:xs) (y:ys) 
			| x < y 	= x : diff xs (y:ys)
			| x > y 	= diff (x:xs) ys
			| otherwise = diff xs ys 


-- Determine if Set A (xs) is a subset of Set B (ys)
subseteq :: (Ord a) => Set a -> Set a -> Bool
subseteq (Set xs) (Set ys) = subset xs ys
	where
		subset [] _ = True 	-- empty set is subset of every set
		subset xs [] = False
		subset xs ys = length (set2list intersection) == length xs
		intersection = intersectS (Set xs) (Set ys)
