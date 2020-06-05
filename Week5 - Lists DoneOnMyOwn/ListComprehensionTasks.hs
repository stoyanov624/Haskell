isPrime :: Integer -> Bool
isPrime 1   = False
isPrime num = (length [x | x <- [2 .. num-1], num `mod` x == 0]) == 0 

isPerfect :: Integer -> Bool
isPerfect num = num == (sum[x | x <- [1 .. num-1], num `mod` x == 0])

primesInRage :: Integer -> Integer -> [Integer]
primesInRage a b = [n |n <- [a .. b], isPrime n]


prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product[n | n <- xs, (sumDivisors n) `mod` k == 0]
    where sumDivisors n = sum [divisor | divisor <- [1..n], n `mod` divisor == 0]

squares :: Integer -> Integer -> Integer -> [(Integer,Integer)]
squares start finish iteration = [(x,x*x) | x <- [start , start + iteration .. finish]] 

type Cylinder = (Double,Double)
getVolume :: [Cylinder] -> [Double]
getVolume cylinders = [pi*r*r*h | (r,h) <- cylinders]

main :: IO()
main = do
    print (isPrime 13)
    print (isPerfect 5)
    print (primesInRage 3 7)
    print (prodSumDiv [1 .. 10] 3)
    print (squares 1 10 1)
    print (getVolume [(5,4)])