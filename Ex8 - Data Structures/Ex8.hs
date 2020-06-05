type Product = (String,Int,Double)

p1,p2,p3,p4 :: Product
p1 = ("Butter",55,6.2)
p2 = ("Bananas", 22, 1)
p3 = ("Bananas", 5, 2)
p4 = ("Olives", 34,0.5)
p5 = ("Apples", 75, 1)
p6 = ("Bananas", 75, 50)

type Shop = [Product]

sh1::Shop
sh1 = [p1,p2,p3,p4,p5]

getPrice :: Product -> Double
getPrice (_,_,c) = c

getTotal :: Shop -> Double
getTotal [] = 0
getTotal (x:xs) = (getPrice x) + getTotal xs

buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "No such product"
buy name' quantity' (x@(name,quantity,price) : xs)
    |name' == name && quantity' < quantity  = (name,quantity-quantity',price) : xs
    |name' == name && quantity' == quantity = xs
    |name' == name && quantity' > quantity  = x:xs
    |otherwise                              = x : buy name' quantity' xs 

getNeeded :: Int -> Shop -> Shop
getNeeded _ [] = []
getNeeded needed (x@(name,quantity,price) : xs) 
    |quantity <= needed = x : getNeeded needed xs
    |otherwise          = getNeeded needed xs

getAverage :: Shop -> Double
getAverage xs = (getTotal xs) / fromIntegral(length xs)

clossestToAverage :: Shop -> String
clossestToAverage xs = name
    where
        (name,_,_) = foldl1 compareProducts xs
        compareProducts p1@(_,_,price1) p2@(_,_,price2) = if abs(price1 - price2) < abs (price2 - average) then p1 else p2
        average = getAverage xs

cheaperAlternatives :: Product -> Shop -> Int
cheaperAlternatives (name,_,price) xs = length [price |(a,_,c) <- xs,a == name  && c < price] 

   
main :: IO()
main  = do
    
    print (getAverage sh1)
    print (clossestToAverage sh1)
    print (cheaperAlternatives  p6 sh1)

