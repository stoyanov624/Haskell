-- примери с вектори
type ShopItem = (String,Int)
i1,i2 :: ShopItem
i1 = ("Salt: 1kg", 139)
i2 = ("Sugar: 0.5kg", 123)

addPair :: (Int,Int,Int,Int,Int) -> Int
addPair (a,b,c,d,f) = a+b+c+d+f

findMin :: Int -> Int -> (Int,Int)
findMin x y
    |x >= y  = (y,x)
    |otherwise = (x,y)

-- примери с списъци
addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [m+n | (m,n) <- pairList]

isEven :: Int -> Bool
isEven x
    |x `mod` 2 == 0 = True
    |otherwise      = False

isDigit :: Char -> Bool
isDigit ch
    |ch >= '0' && ch <= '9'     = True
    |otherwise                  = False

addEvenPairs :: [(Int,Int)] -> [Int]
addEvenPairs evenPairs = [m+n | (m,n) <- evenPairs , isEven n,isEven m]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

x :: [Int]
x = [1 .. 5]

z :: [Int]
z = [6 .. 10]

y :: String
y = "aaaab"

main :: IO()
main = do
    print (snd(i1,i2))
    print (addPair(5,5,5,5,5))
    print (findMin 69 69)
    print [n | n <- x, isEven n]
    print (addPairs [(2,3),(60,9)])
    print (addEvenPairs [(2,4),(3,60)])
    print (tail x)
    print (1:x)
    print (x++z)
    print (length (x++z))
    