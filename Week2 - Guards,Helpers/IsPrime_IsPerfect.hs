isPrime :: Int -> Int -> Bool
isPrime number iteration
    |number <= iteration            = True
    |number `mod` iteration == 0    = False
    |otherwise                      = isPrime number (iteration + 1)

sumDivisors :: Int -> Int -> Int -> Int
sumDivisors number iteration sum
    |number == iteration         = sum
    |number `mod` iteration == 0 = sumDivisors number (iteration + 1) (sum + iteration)
    |otherwise                   = sumDivisors number (iteration + 1) sum

isPerfect :: Int -> Int -> Bool
isPerfect num sum
    |num == sum     = True
    |otherwise      = False
    
main :: IO()
main = do
    print (isPrime 1 2)
    print (isPerfect 7 (sumDivisors 7 1 0))