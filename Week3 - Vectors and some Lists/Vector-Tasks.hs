--Задача 1
addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

--Задача 2
dividePair :: (Int,Int) -> (Int,Int)
dividePair (a,0) = error "Can't divide with 0"
dividePair (a,b) = ((a `div` b) , (a `mod` b))

--Задача 3
type Vector = (Double,Double,Double)
v1,v2 :: Vector
v1 = (2,0,0)
v2 = (1,1,1) 

sumVectors :: Vector -> Vector -> Vector
sumVectors (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

scalarVector :: Vector -> Double -> Vector
scalarVector (x1,y1,z1) number = (x1*number,y1*number,z1*number)

dotProd :: Vector -> Vector -> Double
dotProd (x1,y1,z1) (x2,y2,z2) = (x1*x2) + (y1*y2) + (z1*z2)

magnitude :: Vector -> Double
magnitude (x,y,z) = sqrt(x*x + y*y + z*z)
    
--Задача 4
type Rat = (Int,Int)
rat1 = (8,4)
rat2 = (4,2)

sumRat :: Rat -> Rat -> Rat
sumRat (num1,denum1) (num2,denum2) 
    |denum1 == 0 || denum2 == 0 = error "Dumbass"
    |denum1 == denum2           = (num1+num2,denum1)
    |otherwise                  = (num1*denum2 + num2 * denum2,denum1*denum2) 

multiplyRat :: Rat -> Rat -> Rat
multiplyRat (num1,denum1) (num2,denum2) 
    |denum1 == 0 || denum2 == 0 = error "Dumbass"
    |otherwise                  = (num1*num2,denum1*denum2)

divideRat :: Rat -> Rat -> Rat
divideRat (num1,denum1) (num2,denum2) 
    |denum1 == 0 || denum2 == 0 = error "Dumbass"
    |otherwise                  = (num1*denum2,num2*denum1)

equalRat :: Rat -> Rat -> Bool
equalRat (num1,denum1) (num2,denum2) 
    |denum1 == 0 || denum2 == 0 = error "Dumbass"
    |num1*denum2 == num2*denum1 = True
    |otherwise                  = False


normalizeRat :: Rat -> Rat
normalizeRat (num1,0) = error "Dumbass"
normalizeRat (num1,denum1) = (num1 `div` d,denum1 `div` d)
    where d = (gcd num1 denum1)

sumNormalizeRat :: Rat -> Rat -> Rat
sumNormalizeRat (num1 , denum1) (num2 , denum2)
    |denum1 == 0 || denum2 == 0 = error "Dumbass"
    |otherwise                  = (sumRat (normalizeRat (num1,denum1)) (normalizeRat(num2,denum2)))

  

main :: IO()
main = do
    print (addPair (2,(-2)))
    print (dividePair (3,2))
    print (sumVectors v1 v2)
    print (scalarVector v1 5)
    print (dotProd v1 v2)
    print (magnitude v1)
    print (sumRat rat1 rat2)
    print (multiplyRat rat1 rat2)
    print (divideRat rat1 rat2)
    print (equalRat rat1 rat2)
    print (normalizeRat rat1)
    print (sumNormalizeRat rat1 rat2)
    