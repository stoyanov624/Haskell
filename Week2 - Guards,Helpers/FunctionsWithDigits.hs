countDigits :: Int -> Int -> Int
countDigits 0 count   = count
countDigits num count = countDigits (num `div` 10) (count + 1) 

countDigitsIter :: Int -> Int
countDigitsIter num = helper num 0
    where
        helper 0 count = count
        helper num count  = helper (num `div` 10) (count + 1)

sumDigits :: Int -> Int -> Int
sumDigits number sum
    |number < 1    = sum
    |otherwise      = sumDigits (number `div` 10) (sum + (number `mod` 10)) 

areDigitsAscending :: Int -> Bool
areDigitsAscending number 
    |number < 10                                 = True
    |(number`div` 10) `mod` 10 > number `mod` 10 = False
    |otherwise                                   = areDigitsAscending (number `div` 10) 

main :: IO()
main = do
    print (countDigits 123 0)
    print (countDigitsIter 100)
    print (sumDigits 1111 0)
    print (areDigitsAscending 12)
    
    