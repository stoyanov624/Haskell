--Task 1
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 el xs = el:xs
insertAt n el (x:xs) = x : insertAt (n-1) el xs

--Task2
sublistAt :: Int -> Int -> [a] -> [a]
sublistAt start end xs = take (end - start) (drop start xs)

--Task3
chunkOf :: Int -> [a] -> [[a]]
chunkOf size xs = if length xs <= size then [xs] else take size xs : chunkOf size (drop size xs)

--Task4
isSorted :: [Int] -> Bool
isSorted []         = True
isSorted (x:[])     = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs) 

--Task5 
isAscending :: Int -> Bool
isAscending num = isSorted (numToList num)
    where numToList x = if x < 10 then [x] else numToList (x `div` 10) ++ [x `mod` 10]

--Task6
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO()
main = do
    print (insertAt 2 5 [1,2,3,4])
    print (sublistAt 1 4 [1,2,3,4,5,6])
    print (chunkOf 2 [1,2,3,4,5])
    print (isSorted [2,5,4,5])
    print (isAscending 1234)

    print (merge [1,2,3]  [(-1), 3, 5])