-- Binary Search Tree (BST) Implementation
-- Luke Reichold, 04/2014

data Tree a = EmptyTree	| Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Leaf node with no children --
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree


treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
	| x == a = Node x left right  -- Don't want duplicates --
	| x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)     


findInTree :: (Ord a) => a -> Tree a -> Bool  
findInTree x EmptyTree = False  
findInTree x (Node a left right)  
    | x == a = True  
    | x < a  = findInTree x left  
    | x > a  = findInTree x right


removeFromTree :: (Ord a) => a -> Tree a -> Tree a  
removeFromTree _ EmptyTree = EmptyTree -- if removing from a leaf, replace w/ EmptyTree
removeFromTree x (Node a left right)
	| x < a  	= Node a (removeFromTree x left) right
	| x > a  	= Node a left (removeFromTree x right)
	| otherwise = removeSubtree (Node a left right)


-- Helper function for when node to be removed has children
removeSubtree :: (Ord a) => Tree a -> Tree a
removeSubtree (Node _ left EmptyTree)  = left		-- w/ only a left child
removeSubtree (Node _ EmptyTree right) = right		-- w/ only a right child
removeSubtree (Node _ left right) = (Node newNode left (removeFromTree newNode right))	-- w/ 2 children
	where newNode = minimumElement right


-- Helper function to get smallest element on binary tree
minimumElement :: (Ord a) => Tree a -> a
minimumElement (Node k EmptyTree _) = k
minimumElement (Node _ left _)  = minimumElement left
