null' :: [a] -> Bool
null' [] = True
null' xs = False

head' :: [a] -> a
head' []     = error "List is empty"
head' (x:_)  = x 

tail' :: [a] -> [a]
tail' []    = error "List is empty"
tail' [x]   = error "There is only a head"
tail' (_:xs) = xs

sum' :: [Int] -> Int
sum' []     = 0 
sum' (x:xs) = x + sum' (xs)

lenght' :: [a] -> Int
lenght' [] = 0
lenght' (_:xs) = 1 + lenght' (xs)

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = if (e == x) then True else elem' (e) (xs)

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) (xs)


take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs

minimum' :: [Int] -> Int
minimum' [] = error "Empty list"
minimum' (x:xs) = helper (x:xs) x
    where  
        helper [] min     = min
        helper (x:xs) min = if (x < min) then helper xs x else helper xs min

append' :: [a] -> [a] -> [a]
append' [] ys     = ys
append' (x:xs) ys = x : (append' xs ys) 

reverse' :: [a] -> [a]
reverse' xs = helper [] xs
    where helper :: [a] -> [a] -> [a]
          helper acc []   = acc
          helper acc (x:xs) = helper (x:acc) (xs)  

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs)  = x ++ concat' xs  

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs
 
main :: IO()
main = do
    print (null' [1])
    print (head' [1])
    print (tail' [1 .. 6])
    print (lenght' [2.5])
    print (elem' 2 [1])
    print (drop' 7 [1 .. 6])
    print (take' 3 [1 .. 6])
    print (minimum' [1,2,3])
    print (append' [1,2,3] [4,5,6])
    print (reverse' [1,2,3])
    print (concat' [[1,2,3], [3,4,5]])
    print (zip' [1,2,3] [3,4,5])
  
