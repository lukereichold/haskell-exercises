-- Simple run-length encoding (RLE) compression implementation.
-- https://en.wikipedia.org/wiki/Run-length_encoding
-- Luke Reichold, 04/2014

import Data.Char

-- Helper function to trim whitespace from front and end of String
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


-- Helper function to convert string of form "IntChar" to tuple (Int, Char)
strToTuple :: String -> (Int, Char)
strToTuple s = (n, head rest)
  where [(n, rest)] = (reads :: ReadS Int) s


-- Run-length encoding
-- Input: "Apple", Output: "1A 2p 1l 1e"
rle :: String -> String
rle [] = []
rle (x:xs) = trim $ len ++ x : (" " ++ nextLetter)
	where 	
		len = show (length $ x : takeWhile (==x) xs)
		nextLetter = rle (dropWhile (==x) xs)


-- Run-length decoding
-- Input: "1A 2p 1l 1e", Output: "Apple"
rleInverse :: String -> String
rleInverse [] = []
rleInverse xs = concatMap (uncurry replicate) tupleForm
	where tupleForm = map strToTuple (words xs)