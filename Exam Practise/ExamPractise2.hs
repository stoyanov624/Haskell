--Task1
import Data.List
sumDigits :: Int -> Int
sumDigits num 
    |num < 10  = 0
    |otherwise = (num `mod` 10) + sumDigits (num `div` 10)

interestingNumber :: Int -> Bool
interestingNumber num = num `mod` sum == 0
            where 
                sum = (sumDigits num)

--Task2
sumNumbers :: Int -> Int -> Int
sumNumbers a b = sum [x | x <- [a..b], hasSix x, mod x 4 == 1]
    where
        hasSix 6 = True
        hasSix x
            |x < 10    = True
            |otherwise = (mod x 10) == 6 || hasSix (div x 10)


--Task 3
isAritmeticProg :: [Int] -> Bool
isAritmeticProg []         = True
isAritmeticProg [x1]       = True
isAritmeticProg (x1:x2:x3:xs) = if((x1 - x2) == (x2-x3)) then isAritmeticProg xs else False
        where d = x1-x2

aritmeticProgs :: [[Int]] -> [[Int]]
aritmeticProgs xs = [x | x <- xs, isAritmeticProg x]

--Task4

removeAllDuplicates :: [Int] -> Int -> [Int]
removeAllDuplicates xs duplicate = [x | x <- xs, x /= duplicate]

getUniqueNumbers :: [Int] -> [Int]
getUniqueNumbers [] = []
getUniqueNumbers (x:xs) = if(all (/=x) xs) then x : getUniqueNumbers xs else removeAllDuplicates xs x

sumUniqueNums :: [[Int]] -> Int
sumUniqueNums xs = sum $ concat $ map getUniqueNumbers xs

--Task5

fact :: Int -> Int
fact 0   = 1
fact num = num * fact (num -1) 

funcSinus :: Int -> Double -> Double
funcSinus n x = helper 0 x 0
    where 
        helper iteration x sum = if (iteration >= n) then sum else helper (iteration + 1) x (sum + ((((-1)^iteration) * (x ^ (2*iteration + 1)))) / (fromIntegral (fact (2*iteration + 1))))
        
type Student = String
type Subject = String
type Note = Double

type Record = (Student,Subject,Note)

s1 :: Student
s1 = "Pesho"

s2 :: Student
s2 = "Dragan"

s3 :: Student
s3 = "Ivan"

sub1 :: Subject
sub1 = "Math"

sub2 :: Subject
sub2 = "Biology"

rec1 :: Record
rec1 = (s1,sub1,3)

rec2 :: Record
rec2 = (s3,sub1,4)

rec3 :: Record
rec3 = (s3,sub2,5)

rec4 :: Record
rec4 = (s2,sub2,2)

getSubjects :: [Record] -> [(Subject,Note)]
getSubjects []                 = []
getSubjects ((st,sub,note):rs) = (sub,note) : getSubjects rs

getNotes :: [(Subject,Note)] -> [Note]
getNotes [] = []
getNotes ((sub,note):nts) = note : getNotes nts

thightenSubjects :: (Subject,Note) -> [(Subject,Note)] -> (Subject,Note)
thightenSubjects (sub1,note1) subjects = helper (sub1,note1) subjects 1
    where
        helper (sub1,note1) [] count = (sub1, (note1 / count))
        helper (sub1,note1) ((sub2,note2):sbs) count = if (sub1 == sub2) then helper (sub1,(note1+note2)) sbs (count+1) else helper (sub1,note1) sbs count

hasSubject :: Subject -> [(Subject,Note)] -> Bool
hasSubject sub []             = False
hasSubject sub ((sub2,_):sbs) = sub == sub2 || hasSubject sub sbs 

weakest :: [(Subject,Note)] -> Subject
weakest [(sub1,note1)] = sub1
weakest ((sub1,note1):sbs) = if(all (>note1) (getNotes sbs)) then sub1 else weakest sbs    

hardestSubject :: [Record] -> Subject
hardestSubject records = weakest (helper sbs)
    where
        sbs = getSubjects records
        helper []      = []
        helper ((sub,note):sbs) = if(hasSubject sub sbs) then thightenSubjects (sub,note) sbs : helper sbs else helper sbs
   


--Task 7
toList :: Int -> [Int]
toList num
    |num < 10  = [num]
    |otherwise = num `mod` 10 : toList (num `div` 10)

toInt :: [Int] -> Int
toInt xs = helper xs 0
    where
        helper [] result     = result
        helper (x:xs) result = helper xs (result * 10 + x)

reverseOrdSuff :: Int -> Int
reverseOrdSuff k = toInt (reverse (helper (toList k) []))
    where 
        helper [x] rs = x:rs 
        helper (x1:x2:xs) rs = if (x1 < x2) then helper (x2:xs) (x1:rs) else x1:rs
        
   
main :: IO()
main = do
    
    print $ thightenSubjects (sub1,0) [(sub1,4),(sub2,5),(sub1,5),(sub1,3)] 
    print $ getSubjects [rec1,rec3,rec2,rec4]
    print $ hardestSubject [rec1,rec3,rec2,rec4]