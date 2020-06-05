--data Btree = Empty | Node Int Btree Btree deriving Show

data Btree a = Empty | Node a (Btree a) (Btree a) deriving Show
 
t1 :: Btree Int
t1 = Node 5 (Node 2 Empty 
                    (Node 3 Empty Empty)) 
            (Node 6 Empty Empty)

charTree :: Btree Char
charTree = Node '-' (Node '+' (Node '5' Empty Empty) 
                              (Node '*' (Node '2' Empty Empty)
                              (Node 'x' Empty Empty)))
                    (Node '4' Empty Empty)


--Task 1
size :: Btree a -> Int
size Empty               = 0
size (Node _ left right) = 1 + (size left) + (size right)

height :: Btree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumTree :: Num a => Btree a -> a
sumTree Empty               = 0
sumTree (Node x left right) = x + sumTree(left) + sumTree(right)

sumLeaves :: Num a => Btree a -> a
sumLeaves Empty                = 0
sumLeaves (Node x Empty Empty) = x
sumLeaves (Node _ left right)  = sumLeaves left + sumLeaves right

inorder :: Btree a -> [a]
inorder Empty = []
inorder (Node x left right) = (inorder left) ++ [x] ++ (inorder right)

preorder :: Btree a -> [a]
preorder Empty = []
preorder (Node x left right) =  [x] ++ (preorder left) ++ (preorder right) 

getExpression :: Btree Char -> String
getExpression Empty                = ""
getExpression (Node c Empty Empty) = [c]
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"

instance Eq a => Eq (Btree a) where
    Empty         == Empty            = True
    Node x1 l1 r1 == Node x2 l2 r2    = x1 == x2 && l1 == l2 && r1 == r2 
 
average :: Btree Int -> Double
average tree = (fromIntegral $ sumTree tree) / (fromIntegral $ size tree)

average' :: Btree Int -> Double
average' tree = (fromIntegral $ sum nodes) / (fromIntegral $ length nodes)
        where 
            nodes = inorder tree

getLevel :: Int -> Btree a -> [a]
getLevel _ Empty                = []
getLevel 1 (Node x left right)  = [x]
getLevel k (Node x left right)  = getLevel (k-1) left ++ getLevel (k-1) right 

getLevelsTree :: Btree a -> Btree (a,Int)
getLevelsTree tree = helper tree 0
    where
        helper Empty                 _ = Empty
        helper (Node x left right) lvl = Node (x,lvl) (helper left (lvl + 1)) (helper right (lvl + 1))

mirrorTree :: Btree a -> Btree a
mirrorTree Empty               = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

mapTree :: (a -> b) -> Btree a -> Btree b
mapTree _ Empty = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

main :: IO()
main = do
   print $ size t1
   print $ sumLeaves t1
