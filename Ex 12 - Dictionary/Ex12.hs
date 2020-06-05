import Data(nub)


assocList :: [(Int,Char)]
assocList = [(1,'a'),(2,'b'),(3,'c')]

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' _ [] = error "key does not exist"
lookup' key ((key',assoc) : rest) = if key == key' then assoc else lookup' key rest 


replace :: Eq a => [a] -> [(a,b)] -> [b]
replace xs dictionary = map (\x -> (lookup' x dictionary)) xs 

--По даден граф да се връща списък от върховете му

nodes :: [(Int,Int)] -> [Int]
nodes graph = nub $ foldr (\ (a,b) res -> a:b:res) [] graph

neighbours :: [(Int,Int)] -> Int -> [Int]
neighbours edges node = [b | (a,b) <- edges , a == node]


--предикат който приема граф и списък от върхове и проверява дали списъкът представлява път в графа
isPath :: [(Int) [Int])] -> [Int] -> Bool
isPath _ []         = True
isPath graph [v]    = v `elem` nodes
    where nodes = [v | (v,_) <- graph]
isPath graph(v1:v2:vs) = v2 `elem` (neighbours v1) && isPath graph (v2:vs)
    where neighbours v  = fromJust (lookup v graph)

main :: IO()
main = do

    print $ (lookup' 6 assocList)
    print $ (replace [1,2] assocList)