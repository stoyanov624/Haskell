import Data.Char(isDigit,ord,toUpper,isUpper,isLower,isSpace)
import Data.List(isPrefixOf,group,sort,nub,sortBy)

onlyNumbers :: String -> String
onlyNumbers str = filter isDigit str

digitsSum :: String -> Int
digitsSum "" = 0
digitsSum (c:cs) = if isDigit c then ord c - ord '0' + digitsSum cs else digitsSum cs

digitsSum' :: String -> Int
digitsSum' str = sum (map(\c -> ord c - ord '0') (onlyNumbers str))

capitalize :: String -> String
capitalize str = map toUpper str

isCapitalized :: String -> Bool
isCapitalized "" = True
isCapitalized str = length (filter isLower str) == 0  

isCapitalized' :: String -> Bool
isCapitalized' str = all isUpper str

isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int
countVowels word = length $ filter isVowel word

nOrMoreVowels :: [String] -> Int -> [String]
nOrMoreVowels words n = filter (\ word -> countVowels word >= n) words

tails :: [a] -> [[a]]
tails [] = [[]]
tails ys@(_:xs) = ys : tails xs

isInfixOf' :: String -> String -> Bool
isInfixOf' str1 str2 = any (isPrefixOf str1) (tails str2)

longestSubstring :: String -> Int
longestSubstring str = maximum [length substring | substring <- group str] 

firstWord :: String -> String
firstWord ""         = ""
firstWord (' ':cs)   = firstWord cs
firstWord (c:' ':cs) = [c]
firstWord (c:c':cs)  = c : firstWord (c':cs)
firstWord cs         = cs

words' :: String -> [String]
words' ""         = []
words' (' ':cs)   = words' cs
words' [c]        = [[c]]
words' (c:' ':cs) = [c] : words' cs
words' (c:c':cs)  = (c:rs):rss
    where rs:rss = words' (c':cs)

unwords' :: [String] -> String
unwords' []     = ""
unwords' [w]    = w
unwords' (w:ws) = w ++ " " ++ unwords' ws

tightenString :: String -> String
tightenString ""             = ""
tightenString (' ':cs)       = tightenString cs
tightenString (c:' ':' ':cs) = tightenString (c:' ':cs)
tightenString (c:' ':c':cs)  = c : ' ' : tightenString (c':cs)
tightenString (c:cs)         = c : tightenString cs

tightenString' :: String -> String
tightenString' cs = unwords' (words' cs)

calculateFrequencyTable :: String -> [(Char,Int)]
calculateFrequencyTable ""  = []
calculateFrequencyTable str = sortBy (\ (_, cnt1) (_, cnt2) -> compare cnt2 cnt1) pairs
                where pairs = [(letter,length $ filter (== letter) str) | letter <- sort (nub str)]





main :: IO()
main = do
    {-print $ onlyNumbers "123asddasdsa12415"
    print $ digitsSum "123"
    print $ digitsSum' "123"
    print $ capitalize "aaaaa"
    print $ isCapitalized "AaAA"

    print $ nOrMoreVowels ["cat","dog","leper"] 1
    print $ firstWord "    asdasdsada"
    print $ words' " abv asd ads"
    print $ unwords' ["abv","aaa","cc"]

    print $ tightenString' "Sadsad dasd dasdad    dsadas"
    print $ calculateFrequencyTable "adgvfdfafew"-}

    print $ multiply 3
