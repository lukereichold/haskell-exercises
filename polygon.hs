-- Point, Line, and Polygon definitions and operations
-- Luke Reichold, 04/2014

import Data.List (nub)

type Point = (Double, Double)
type Polygon = [Point]
type LineSegment = (Point, Point)

-- Calculates the distance between 2 Points
dist :: Point -> Point -> Double
dist p1 p2 = sqrt $ x'^2 + y'^2
	where
		x' = fst p2 - fst p1
		y' = snd p2 - snd p1


-- Determines whether given Point is on line segment (a, b)
onLineSegment :: Point -> Point -> Point -> Bool
onLineSegment p a b = (-eps < f && f < eps)
	where 
		f = dist a p + dist p b - dist a b
		eps = 0.00001


-- Tests whether polygon is valid
isValid :: Polygon -> Bool
isValid [] = False
isValid ps = (length (nub ps) >= 3)


-- Calculates the perimeter of a valid polygon (Assuming points given are adjacent!)
perimeter :: Polygon -> Double
perimeter ps
	| (isValid ps)	= foldl (+) 0 (zipWith dist ps (shift ps))
	| otherwise 	= error "Not a valid polygon"


-- Checks if given point is on polygon border
onPolygonBorder :: Point -> Polygon -> Bool
onPolygonBorder a ps
	| (isValid ps)	= any checkSide (breakPolygonToLines ps)
	| otherwise 	= error "Not a valid polygon"
	where checkSide line = onLineSegment a (fst line) (snd line) 


-- Helper function: breaks polygon into line segments
breakPolygonToLines :: Polygon -> [LineSegment]
breakPolygonToLines ps = (zip ps (shift ps))


-- Helper function: shifts every element in list (circularly to the right by 1). 
-- Input of [1,2,3] gives output of [2,3,1]
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]
