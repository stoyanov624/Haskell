myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b) 
main :: IO()
main = do
    print (myGcd 12 16)