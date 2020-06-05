myFactIter :: Integer -> Integer
myFactIter num = helper num 1
    where 
        helper 0 acc = acc
        helper num acc = helper (num -1) (acc*num)
main :: IO()
main = do
    print (myFactIter 5)    