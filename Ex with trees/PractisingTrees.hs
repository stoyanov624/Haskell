data Btree a = Empty | Node a (Btree a) (Btree a) deriving Show

-- представяне на аритметичен израз чрез дърво - по листата ще има стойности (числа, неизвестни), 
-- а по вътрешните върхове - инфиксни операции (+, -, *, /, ^)
expression :: Btree Char                                            --      -
expression = Node '-' (Node '+' (Node '5' Empty Empty)              --     / \
                                (Node '*' (Node '2' Empty Empty)    --    +   4
                                          (Node 'x' Empty Empty)))  --   / \
                      (Node '4' Empty Empty)                        --  5   *
                                                                    --     / \
                                                                    --    2   x

data Colour = Red | Green | Blue deriving (Read,Show,Eq)

colourTree :: Btree Colour
colourTree = Node Blue (Node Red Empty (Node Green Empty Empty)) 
                        (Node Red Empty (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)))


t1 :: Btree Int
t1 = Node 5 (Node 2 Empty 
    (Node 3 Empty Empty)) 
             (Node 6 Empty Empty)

t2 :: Btree Int
t2 = Node 809 (Node 2 Empty 
    (Node 3 Empty Empty)) 
             (Node 6 Empty Empty)


--Упражнение 10
size :: Btree a -> Int
size Empty               = 0
size (Node _ left right) = 1 + (size left) + (size right)

height :: Btree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumTree :: Btree Int -> Int
sumTree Empty               = 0
sumTree (Node x left right) = x

sumLeaves :: Btree Int -> Int
sumLeaves Empty                = 0
sumLeaves (Node x Empty Empty) = x
sumLeaves (Node x left right)  = (sumLeaves left) + (sumLeaves right)

equalTrees :: Eq a => Btree a -> Btree a -> Bool
equalTrees Empty Empty                                   = True
equalTrees (Node x1 left1 right1) (Node x2 left2 right2) = x1 == x2 && equalTrees left1 left2 && equalTrees right1 right2

inorder :: Btree a -> [a]
inorder Empty               = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

preorder :: Btree a -> [a]
preorder Empty               = []
preorder (Node x left right) = preorder right ++ [x] ++ preorder left

order :: Btree a -> [a]
order Empty               = []
order (Node x left right) = [x] ++ order left ++ order right

instance Eq a => Eq (Btree a) where
    Empty           == Empty          = True
    Node x1 l1 r1  == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2 

average :: Btree Int -> Double
average tree = (fromIntegral $ sumLeaves tree) / (fromIntegral $ size tree)

getLevel :: Btree a -> Int -> [a]
getLevel Empty _                     = []
getLevel (Node x left right) 1       = [x]
getLevel (Node x left right) level   = getLevel left (level - 1) ++ getLevel right (level - 1)

getLevelsTree :: Btree a -> Btree (a,Int)
getLevelsTree bt = helper bt 1
    where 
        helper Empty _                       = Empty
        helper (Node x left right) iteration = Node (x,iteration) (helper left (iteration + 1)) (helper right (iteration + 1))

mirrorTree :: Btree a -> Btree a
mirrorTree Empty               = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

mapTree :: Btree a -> (a -> b) -> Btree b
mapTree Empty _                  = Empty
mapTree (Node x left right) func = Node (func x) (mapTree left func) (mapTree right func)

getExpression :: Btree Char -> String
getExpression tree = inorder tree

--Упражнение 11


data NTree a = NEmpty | NNode a [(NTree a)]  
t4 :: NTree Int                               --      1
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --     / \
                        (NNode 4 [NEmpty]),   --    2   6
                        (NNode 5 [NEmpty])]), --   /|\  |
              (NNode 6 [(NNode 7 [NEmpty])])] --  3 4 5 7

t5 :: [(Int, [Int])]
t5 = [(4, [2, 5]), (2, [1, 3])])
                                    --     4
                                    --    / \
                                    --   2   5
                                    --  / \
                                    -- 1   3


nTreeSize :: NTree a -> Int
nTreeSize NEmpty = 0
nTreeSize (NNode _ subtrees) = 1 + sum (map nTreeSize subtrees)
        
main :: IO()
main = do

    print $ inorder t1
    print $ average t1
    print $ getLevel t1 1
    print $ getLevelsTree t1
    print $ mirrorTree t1
    print $ mapTree t1 (\x -> x+1)
    print $ getExpression expression
    print $ nTreeSize t4