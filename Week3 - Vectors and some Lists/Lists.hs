--Задача 0
null' :: [a] -> Bool
null' []  = True
null' _   = False

head' :: [a] -> a
head' [] = error "No elements!"
head' (x:_)  = x

tail' :: [a] -> [a]
tail' []      = error "No elements!"
tail' (_:xs)  = xs

sum' :: [Int] -> Int
sum' xs = if null xs then 0 else head xs + sum'(tail xs)

sum'' :: [Int] -> Int
sum'' []   = 0
sum'' (x:xs) = x + sum xs

lenght' :: [a] -> Int
lenght' [] = 0
lenght' (_:xs) = 1 + lenght' xs

elem' :: Int -> [Int] -> Bool
elem' _ []       = False
elem' num (x:xs) = num == x || elem' num xs

take' :: Int -> [a] -> [a]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs  = xs
drop' _ []  = []
drop' n (_:xs) = drop' (n-1) xs

min' :: [Int] -> Int
min' (x:xs) = helper xs x
    where 
        helper [] min = min
        helper (x:xs) min = if (min > x) then helper xs x else helper xs min

append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x : append' xs ys

sublistBetween :: Int -> Int -> [a] -> [a]
sublistBetween start end xs = take (end-start) (drop start xs) 

isSorted :: [Int] -> Bool
isSorted []         = True
isSorted [_]        = True
isSorted (x1:x2:xs) = x1 < x2 && isSorted (x2:xs)





x :: [Int]
x = [5,3,4,5]

y :: [Int]
y = [5,3,4,5]

main :: IO()
main = do
    print (null' x)
    print (head' x)
    print (tail' x)
    print (sum' x)
    print (elem' 123 x)
    print (take' 3 x)
    print (drop' 3 x)
    print (min' x)
    print (append' x y)
    print (sublistBetween 2 3 x)
    print (isSorted [1 .. 5])