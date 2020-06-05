--Task6
isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = helper (a:as) (b:bs) (a-b)
    where 
        helper [] [] _         = True
        helper [a] [b] d       = if(a-b == d) then True else False
        helper (a:as) (b:bs) d = if(a-b /= d) then False else helper as bs d 
 
--Task7
isTriangular :: [[Int]] -> Bool
isTriangular (x:xs) = helper (x:xs) 0 
    where 
        helper [] _             = True
        helper (x:xs) iteration = if(all (==0) (take iteration x)) then helper xs (iteration+1) else False

--ExtraTask1
alternativeSign :: [Int] -> Bool
alternativeSign []           = True
alternativeSign [x1]         = True
alternativeSign [x1,x2]      = if( x1*x2 > 0) then False else True
alternativeSign (x1:x2:xs)   = if( x1*x2 > 0) then False else alternativeSign xs

--ExtraTask2
isAritmeticProgression :: [Int] -> Bool
isAritmeticProgression (x1:x2:xs) = helper (x1:x2:xs) (x2-x1)
    where 
        helper []  _        = True
        helper [x1] _       = True
        helper [x1,x2] d    = if((x2-x1) /= d) then False else True
        helper (x1:x2:xs) d = if((x2-x1) /= d) then False else helper xs d

--ExtraTask3
countOcc :: Eq a => a -> [a] -> Int
countOcc x ys = length (filter (==x) ys)

lettersInWord :: [Char] -> [(Char,Int)]
lettersInWord []     = []
lettersInWord (x:xs) = (x,(countOcc x xs)+1) : lettersInWord (filter (/=x) xs)        

main :: IO()
main = do
    print (isTriangular [[1,2,3,4,3,4],
                         [0,1,0,0,3,4],
                         [0,0,3,4,2,3],
                         [0,0,0,1,3,4],
                         [0,0,0,0,2,3],
                         [0,0,0,0,0,4]])

    print (isAritmeticProgression [9,7])
    print (alternativeSign [(-1),(-2)])
    print (isImage [1,2,3] [2,2,3])

    print(lettersInWord "Hello, World!")