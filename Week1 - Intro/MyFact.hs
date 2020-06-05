myFact :: Integer -> Integer
myFact num
    |num == 0   = 1
    |otherwise  = num * myFact(num - 1)
main :: IO()
main = do
    print (myFact 1)