--Task 1
incrementAllby :: [Int] -> Int -> [Int]
incrementAllby xs n = map (\x -> x+n) xs

incrementAllby' :: [Int] -> Int -> [Int]
incrementAllby' xs n = map (+n) xs

multiplyAllby :: [Int] -> Int -> [Int]
multiplyAllby xs n = map (*n) xs

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = filter (<n) xs

--Task 2
splitbyParity :: [Int] -> ([Int],[Int])
splitbyParity xs = (filter even xs , filter odd xs)


--Task 3
partition' :: Eq a => (a -> Bool) -> [a] -> ([a],[a])
partition' f xs = (filter f xs, filter (not.f) xs)  

--Task 4
splitbyParity' :: [Int] -> ([Int],[Int])
splitbyParity' xs = partition' even xs 

--Task5 
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller,larger) = partition' (<p) xs 

--Task6
isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = helper (a:as) (b:bs)
    where
        d = a - b
        helper [] []         = True
        helper [a] [b]       = if(a-b == d) then True else False
        helper (a:as) (b:bs) = if(a-b /= d) then False else helper as bs 
 
--Task7
isTriangular :: [[Int]] -> Bool
isTriangular []     = True
isTriangular matrix = helper matrix 0 
    where 
        helper [] _             = True
        helper (x:xs) iteration = if(all (==0) (take iteration x)) then helper xs (iteration+1) else False

--ExtraTask1
alternativeSign :: [Int] -> Bool
alternativeSign []           = True
alternativeSign [x1]         = True
alternativeSign (x1:rest@(x2:xs))   = if( x1*x2 > 0) 
                                      then False 
                                      else alternativeSign rest

--ExtraTask2
isAritmeticProgression :: [Int] -> Bool
isAritmeticProgression []                  = True
isAritmeticProgression [x1]                = True
isAritmeticProgression (x1:x2:xs) = helper (x1:x2:xs)
    where
        d = x2-x1
        helper []                  = True
        helper [x1]                = True
        helper (x1:rest@(x2:xs))   = if((x2-x1) /= d) then False else helper rest 

--ExtraTask3

countOcc :: Eq a => a -> [a] -> Int
countOcc x ys = length (filter (==x) ys)

lettersInWord :: [Char] -> [(Char,Int)]
lettersInWord []     = []
lettersInWord (x:xs) = (x,(countOcc x xs)+1) : lettersInWord (filter (/=x) xs)        

main :: IO()
main = do
    print (incrementAllby [1,2,3] 3) 
    print (multiplyAllby [1,2,3] 3) 
    print (filterSmallerThan [1,2,3] 3) 
    print (splitbyParity [1,2,3,4,5,6,7,8,9,10])

    print (partition' odd [1,2,3,4,5,6,7,8,9,10]) 
    print (splitbyParity' [1,2,3,4,5,6,7,8,9,10]) 
    print (quickSort [3,4,2,6,7,1,3,4]) 

    print (isTriangular [[1,2,3,4,3,4],
                         [0,1,0,0,3,4],
                         [0,0,3,4,2,3],
                         [0,0,0,1,3,4],
                         [0,0,0,0,2,3],
                         [0,0,0,0,0,4]])

    print (isAritmeticProgression [9,2])
    print (alternativeSign [(1),(-2),3,(-4)])
    print (isImage [1,2,3] [2,2,3])

    print(lettersInWord "FMI")

    
   

    

   
    
