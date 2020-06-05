removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst elem (x:xs)
    |elem == x = (drop 0 xs)
    |otherwise = x : removeFirst elem xs

removeAll :: Eq a => a -> [a] -> [a]
removeAll elem xs = [n | n <- xs, n /= elem]

containsEl :: Eq a => a -> [a] -> Bool
containsEl elem []     = False
containsEl elem (x:xs) = elem == x || containsEl elem xs
   
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates (x:xs) = helper (x:xs) (x:[])
    where   
        helper [] rs = reverse rs
        helper (x:xs) (r:rs) = if (not(containsEl x (r:rs))) then helper xs (x:(r:rs)) else helper xs (r:rs)

removeDuplicates' :: Eq a => [a] -> [a]
removeDuplicates' [] = []
removeDuplicates' (x:xs) = x : removeDuplicates' (removeAll x xs)  

prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = xs == take (length xs) ys 

coutnOcc :: Eq a => [a] -> [a] -> Int
coutnOcc subxs xs = helper xs 0
    where
        helper [] count            = count
        helper curr@(_:rest) count = if prefix subxs curr then helper rest (count+1) else helper rest count 
   
       

main :: IO()
main = do
    print (removeFirst 5 [1,2,3,4])
    print (removeAll 5 [1,5,5,5,4])
    print (removeDuplicates [1,1,1,5,5,5,2,2,2,5,5,3])
    print (prefix [2,2,2] [1,1,1,5,5,5,2,2,2,5,5,3])
    
    print (coutnOcc [1] [1,2,1,1])
  
    
