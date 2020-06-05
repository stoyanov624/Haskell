--помощна функция за Задача 3
isPrime :: Integer -> Bool
isPrime number = helper number 2
    where 
        helper number iteration
            |number <= iteration            = True
            |number `mod` iteration == 0    = False
            |otherwise                      = helper number (iteration + 1)

--помощна функция за Задача 1
sumSequence :: Integer -> Integer
sumSequence num = helper num 0
    where 
        helper (-1) sum  = sum
        helper num sum   = helper (num - 1) (sum + 2^num)
-- Задача 1  
findSum :: Integer -> Integer -> Integer -> Integer
findSum a b n 
    |n <= 3     = error " n must be > 3 "
    |otherwise  = helper a b n 0 0
    where
        helper a b n sum 3         = sum
        helper a b n sum iteration = helper a b (n-1) (sum + b*(sumSequence (n-1)) + a) (iteration + 1)

-- Задача 2
isSquare :: Int -> Bool
isSquare number = helper number 2
    where
        helper number iteration
            |number < 1                    = error "Number not natural"
            |number == iteration           = False
            |number == 1                   = True
            |number == iteration*iteration = True
            |otherwise                     = helper number (iteration + 1) 

--Задача 3
isSpecial :: Integer -> Int -> Bool
isSpecial number k 
    |number <= 10 = error " Number must be > 10 "
    |otherwise = helper number k
        where helper number k
                 |number `div` 10 <= 0                        = True
                 |(isPrime (number `mod` (10^k))) == False    = False
                 |otherwise                                   = helper (number `div` 10) k 
    

main :: IO()
main = do
    print (findSum 5 3 5)
    print (isSquare 5)
    print (isSpecial 131 3)
    
   