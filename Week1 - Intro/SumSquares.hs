squaresAverage :: Int -> Int -> Double
squaresAverage a b = (fromIntegral(a*a) + fromIntegral(b*b)) / 2
main :: IO()
main = do
    print (squaresAverage 6 6)