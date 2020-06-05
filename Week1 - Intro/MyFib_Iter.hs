myFibonIter :: Integer -> Integer
myFibonIter num = helper num 0 1
    where
        helper 0 prev cur = cur
        helper 1 prev cur = cur
        helper num prev cur = helper (num-1) cur (cur+prev)
main :: IO()
main = do
    print (myFibonIter 5)