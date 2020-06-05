isInside :: Int -> Int -> Int -> Bool
isInside a b x = if(x>= 3 && x <= 6) then True else False
main :: IO()
main = do
    print (isInside 3 6 7)
