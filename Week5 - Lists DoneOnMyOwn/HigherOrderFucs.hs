--Task 1
inside :: Int -> Int -> (Int -> Bool)
inside a b = isInside  
    where isInside x = a <= x && x <= b

inside' :: Int -> Int -> (Int -> Bool)
inside' a b = \ x -> a <= x && x <= b 

--Task 2
twice :: (a -> a) -> (a -> a)
twice f = (f . f)

--Task 3
iter :: Int -> (a -> a) -> (a -> a)
iter 0 _ = id
iter n f = f . iter (n-1) f

--Task 4,5
plusOne :: Int -> Int
plusOne x = x+1

square :: Int -> Int
square x = x*x

xSquaredPlusOne :: Int -> Int
xSquaredPlusOne x = (plusOne . square)x

xPlusOneSquared :: Int -> Int
xSquaredPlusOne x = (square . plusOne)x
    
main :: IO()
main = do
   print ((inside 1 6)3)
   print ((inside' 1 6)3)
   print (twice (\x -> x*1)4)
   print (iter 100 (\x -> x*2)4)
   print (xSquaredPlusOne 5)

