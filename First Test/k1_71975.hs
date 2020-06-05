--Task 1

checkNotMod :: Int -> [Int] -> Bool
checkNotMod num []     = True
checkNotMod num (x:xs) = if (mod x num == 0) then False else checkNotMod num xs

checkSequence :: [Int] -> Bool
checkSequence []         = True
checkSequence [x]        = True
checkSequence (x1:x2:xs) = if(x1 < x2 && checkNotMod x1 (x2:xs)) then checkSequence (x2:xs) else False  

--Task 2
checkForeach :: Int -> [Int] -> Int -> [(Int,Int)]
checkForeach num [] _          = []
checkForeach num (x:xs) summed = if (num * x == (summed-x-num)) then [(num,x),(x,num)] ++ checkForeach num xs summed else checkForeach num xs summed

removeNb :: Int -> [(Int,Int)]
removeNb 0   = []
removeNb num = concat ((helper ns) (sum ns))  
    where
        ns                   = [1 .. num]
        helper [] _          = []
        helper (n:ns) summed = checkForeach n ns summed : helper ns summed 

--Task 3

type Point = (Double,Double)

line :: Point -> Point -> (Double -> Double)
line (x1,y1) (x2,y2) = formula
    where 
        formula x = y1 + (x-x1) * (y2-y1)/(x2-x1) 

{-liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn func = checkFunc
    where
        checkFunc (x,y) =   -} 
        
main ::IO()
main = do
    
    print $ checkSequence [11, 14, 14, 29, 31]
    print $ removeNb 101
    print $ line (0,0) (1,1) 5

