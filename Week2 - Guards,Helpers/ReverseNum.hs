reverseNum :: Int -> Int -> Int
reverseNum number result
    |number < 1     = result
    |otherwise      = reverseNum (number `div` 10) (result * 10 + (number`mod` 10))
main :: IO()
main = do
    print (reverseNum 420 0)