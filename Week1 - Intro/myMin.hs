myMin :: Int -> Int -> Int
myMin x  y 
    |x <= y = x
    |y <= x = y

main :: IO()
main = do
    print (myMin 10 (-10))