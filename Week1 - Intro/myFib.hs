myFib :: Integer -> Integer
myFib num
    |num == 0   = num
    |num == 1   = num
    |otherwise  = myFib(num - 1) + myFib(num - 2)
main :: IO()
main = do
    print (myFib 6)
