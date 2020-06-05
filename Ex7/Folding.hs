rev :: [a] -> [a]
rev lst = foldr (\x xs -> xs ++ [x]) [] lst

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert el xs@(y:ys) = if (el < y) then el : xs else y : insert el ys

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insert [] xs
 
type Point = (Double, Double)

closestPoint :: [Point] -> (Point -> Point)
closestPoint ps (px,py) = foldr1 chooseClosest ps
                where
                    chooseClosest p1 p2 = if(distanceToP p1) < (distanceToP p2) then p1 else p2
                    distanceToP (x,y) = sqrt((px-x)^2 + (py-y)^2)

main::IO()
main = do
    print (foldr (+) 1 [1,2,3,4])
    print (foldl (+) 1 [1,2,3,4])

    print(foldr (-) 5 [1,2,3,4])
    print(foldl (-) 5 [1,2,3,4])

    print (foldr1 (+) [1,2,3,4])
    print (foldl1 (+) [1,2,3,4])

    print (foldr1 (-) [1,2,3,4])
    print (foldl1 (-) [1,2,3,4])

    print (foldr (\x y -> (x+y)/2) 54[12,4,10,6])

    print(concat ["kur","sdadas"])
    print (foldr (\x y -> concat["(",x,"+",y,")"]) "0" ["first","second","third"])
    print (foldl (\x y -> concat["(",x,"+",y,")"]) "0" ["first","second","third"])

    print (insert 0 [1,2,4])
    print (insertionSort [1,3,4,5,6,7,8,9])
    print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (0.9, 2))
   

    