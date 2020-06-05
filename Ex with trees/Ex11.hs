data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

data Color = Red | Green | Blue deriving (Read,Show,Eq)

colorTree :: BTree Color
colorTree = Node Blue 
            (Node Red (Node Green Empty Empty) Empty)
            (Node Red (Node Blue (Node Green Empty Empty)
                                 (Node Red Empty Empty))


maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree = helper tree 1
    where
        helper Empty _                  = 0
        helper (Node Blue left right) currDepth = maximum [currDepth, helper left (currDepth + 1), helper right (currDepth + 1)]
        helper (Node _    left right)           = max (helper left (currDepth + 1)) (helper right (currDepth + 1))


maxDepthColorNode :: BTree Color -> Color -> Int
maxDepthColorNode tree color = helper tree 1
    where
        helper Empty _ = 0
        helper (Node c left right) currDepth
            |c == color = maximum [currDepth,helper left(currDepth + 1), helper right(currDepth + 1)]
            |otherwise = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

data Ntree a = NEmpty | NNode a [(Ntree a)]

t4 :: Ntree Int
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]), NNode 4 [NEmpty]),(NNode 5 [NEmpty])]),(NNode 6 [(NNode 7 [NEmpty])])]

nTreeSize :: Ntree a -> Int
nTreeSize NEmpty = 0
nTreeSize (NNode _ subTree) = 


main :: IO()
main = do

    print & maxDepthColorNode colorTree Blue 