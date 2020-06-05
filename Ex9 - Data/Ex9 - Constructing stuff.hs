data Shape = Circle Double 
             | Rectangle Double Double 
             | Triangle Double Double Double 
             | Cylinder Double Double

instance Show Shape where
    show(Circle radius) = "A circle with a radius " ++ show radius
    show (Rectangle sideA sideB) = "A rectangle with sideA " ++ show sideA ++ " and sideB " ++ show sideB             
    show (Triangle sideA sideB sideC) = "A triangle with sideA " ++ show sideA ++ " , SideB " ++ show sideB ++  " and sideC " ++ show sideC

circle, rectangle,cylinder,triangle :: Shape
circle = Circle 3
rectangle = Rectangle 4 5
triangle = Triangle 1 1 1
cylinder = Cylinder 3 3

--Task 1
perimeter :: Shape -> Double
perimeter (Circle radius)              = 2* pi * radius
perimeter (Rectangle sideA sideB)      = 2* (sideA + sideB)
perimeter (Triangle sideA sideB sideC) = sideA + sideB + sideC
perimeter _                            = error "Not a shape!" 

area :: Shape -> Double
area (Circle r)                  = pi* r*r
area (Rectangle a b)             = a*b
area tr@(Triangle a b c)         = sqrt $ p* (p - a)* (p - b)* (p - c)
    where p = (perimeter $ tr)/2
area _                           = error "Not a shape!"

isRound :: Shape -> Bool
isRound (Cylinder _ _) = True
isRound (Circle _)     = True
isRound _              = False


--Task 2
sumAreas :: [Shape] -> Double
sumAreas shapes = foldr1 (+) (map area shapes)

sumAreas2 :: [Shape] -> Double
sumAreas2 = foldr1 (+) . map area

--Task 3
biggestShape :: [Shape] -> Shape
biggestShape shapes = foldr1 (\ sh1 sh2 -> if area sh1 >= area sh2 then sh1 else sh2) shapes

--Task 4
data Point = P2 Double Double | P3 Double Double Double deriving Show

printPoint :: Point -> String
printPoint (P2 x1 x2)    = "[" ++ show x1 ++ "," ++ show x2 ++ "]"
printPoint (P3 x1 x2 x3) = "[" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "]"


instance Eq Point where
    (P2 x1 y1)    == (P2 x2 y2)    = x1 == x2 && y1 == y2
    (P3 x1 y1 z1) == (P3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2
    _             == _             = error "Not in the same dimension!"

--Task5
distance :: Point -> Point -> Double
distance (P2 x1 y1) (P2 x2 y2)       = sqrt $ (x2-x1)^2 + (y2-y1)^2
distance (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 
distance _              _            = error "Invalid Points1"

--Task6
getClosestPoint :: [Point] -> Point -> Point
getClosestPoint points p = foldr1 (\ p1 p2 -> if distance p p1 >= distance p p2 then p1 else p2) points

main :: IO()
main = do

    print(Rectangle 4 5)
    print(Triangle 4 5 6)
    print (sqrt 4 + 3 + 9)
    print(sqrt (4 + 3 + 9))
    print(sqrt $ 4 + 3 + 9)
    print $ 4 + 5

    print $ printPoint (P3 2 3 4)