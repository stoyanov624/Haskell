import Data.List
data Btree a = Empty | Node a (Btree a) (Btree a) deriving (Read,Show,Eq)

t1 :: Btree Char                                        --      
t1 = Node 'a' (Node 'c' (Node 'f' (Empty) Empty)        --         a
                        (Node 'd' Empty Empty))         --        / \  
              (Node 'b'  Empty                          --      c     b
              (Node 'e' Empty Empty))                   --    /  \     \
                                                        --   f    d     e 


t2 :: Btree Char                                           --     a
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)             --    / \
               Empty)                                      --   c   b
     (Node 'b' Empty Empty)                                --  / \
                                                           -- d   f


--Task2

suffixes :: String -> [String]
suffixes []         = []
suffixes xs         = helper 0
    where helper iteration = if (iteration == length xs) then [] else drop (iteration) xs : helper (iteration + 1)
  
genWords:: Btree Char -> [String]
genWords tree = concat $ map suffixes (helper tree)
    where 
        helper Empty                = []
        helper (Node x Empty Empty) = [[x]]
        helper (Node x left right)  = map (x:) (helper left ++ helper right)

--Task 1

containsWord :: Btree Char -> String -> Bool
containsWord tree str = elem str $ genWords tree

--Task 3

allContain :: [Btree Char] -> [String]
allContain []      = []
allContain ts      = getDuplicates ws
    where 
        ws = concat [genWords t| t <- ts]
        getDuplicates [] = []
        getDuplicates (x:xs) = if(any (==x) xs) then x : getDuplicates xs else getDuplicates xs
        
--Task 4

data Ntree = Nil | NNode Int [Ntree] deriving (Show)


t3 :: Ntree                         --      1
t3 = NNode 1 [NNode 3 [],           --   / / \ \
 NNode 5 [],                        --  3  5  7 9
 NNode 7 [],
 NNode 9 []]
                                        --   7
t4 :: Ntree                             --   |
t4 = NNode 7 [NNode 9 [NNode 5 [],      --   9
 NNode 2 []]]                           --  / \
                                        -- 5   2

getFathers :: Ntree -> [Int]
getFathers Nil                = []
getFathers (NNode _ [])       = []
getFathers (NNode x subtrees) = foldl (++) [x] (map getFathers subtrees)

getKids :: Ntree -> [Int]
getKids Nil                = []
getKids (NNode _ [y])      = getFathers y ++ getKids y 
getKids (NNode x [])       = [x]
getKids (NNode _ subtrees) = foldl1 (++) (map getKids subtrees)

kidsInList :: Ntree -> [[Int]]
kidsInList Nil                 = []
kidsInList (NNode _ [y])       = [getFathers y] ++ [getKids y] 
kidsInList (NNode x subtrees)  = [foldl1 (++) (map getKids subtrees)]

isGracefull :: Ntree -> Bool
isGracefull tree = if (any (odd) (concat (helper ps ks))) then False else True 
        where
              ps                   = getFathers tree
              ks                   = reverse (kidsInList tree)
              helper []  _         = [] 
              helper (p:ps) (k:ks) = (map (p-) k) : helper ps ks

--Task 5
t5 :: Btree Int                                 --    8
t5 = Node 8 (Node 3 (Node 1 Empty Empty)        --   / \
                    (Node 4 Empty Empty))       --  3  10
            (Node 10 (Node 9 Empty Empty)       -- / \ / \
                     (Node 14 Empty Empty))     -- 1 4 9 14

t6 :: Btree Int 
t6 = Node 8 
        (Node 3 (Node 1 Empty Empty)
                (Node 4 Empty Empty)) 
        (Node 10 (Node 5 Empty Empty) 
                 (Node 14 Empty Empty))

t7 :: Btree Int
t7 = Node 8 
        (Node 3  (Node 5 Empty Empty)
                 (Node 6 Empty Empty))
        (Node 10 (Node 5 Empty Empty)
                 (Node 14 Empty Empty))


inorder :: Btree a -> [a]
inorder Empty               = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

isBinarySearchTree :: Btree Int -> Bool
isBinarySearchTree (Node x left right) = all (<x) (inorder left) && all (>x) (inorder right)  
       

main :: IO()
main = do
    
   print $ genWords t1
   print $ genWords t2
   print $ containsWord t1 "acd"
   print $ allContain [t1, t2]
   print $ isGracefull t4
   print $ isBinarySearchTree t5