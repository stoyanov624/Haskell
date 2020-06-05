--Task1
generate :: Double -> Int -> [Double]
generate p n = if (n <= 0) then error "Invalid input" else reverse (helper xs (length xs))
    where 
        xs = [(1/(fromIntegral(m)**p))|m <- [1 .. n]]
        helper _ 0 = []
        helper xs iteration = (sum (take iteration xs)) : helper xs (iteration - 1)

--Task2
isSquare :: Int -> Bool
isSquare num = (length [x | x <- [1 .. num], x*x == num]) >= 1

sumDivisors :: Int -> Int
sumDivisors num =  sum [d^2 |d <- [1 .. num],num `mod` d == 0]

listSquares :: Int -> Int -> [(Int,Int)]
listSquares a b
        |a > b     = error "Invalid input"
        |otherwise = [(a,sumDivisors a) |a <- [a .. b] , isSquare a]

--Task3
type Point = (Double,Double)
inCircle :: Point -> Double -> Point -> Bool
inCircle (c1,c2) r (p1,p2) = (sqrt((p1-c1)^2 + (p2 - c2)^2)) <= r

splitPoints :: Point -> Double -> [Point] -> ([Point],[Point])
splitPoints (x,y) r ps = (filter (inCircle (x,y) r) ps , filter (not . (inCircle (x,y) r)) ps) 

--Task4

type Person = (Int,String,String)
ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"),
 (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]

type Account = (Int,Int,Double)
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2),
 (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance ([],_) _ = 0
getAverageBalance (_,[]) _ = 0
getAverageBalance (as,ps) f = if (length xs == 0 || length ms == 0) then 0 else (sum ms) / fromIntegral(length ms)
                where 
                    xs = filter f ps
                    ms = [m |(_,n,m) <- as, (l,_,_) <- xs, l == n]
                    
 
main :: IO()
main = do
    print (generate 1 3)
    print (generate 0.1 5)

    print (listSquares 1 30)
    print (listSquares 250 300)
    
    print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    print (splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    
    print (getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas"))
    print(getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P'))