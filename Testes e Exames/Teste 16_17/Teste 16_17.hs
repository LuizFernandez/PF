--Teste 16_17

import Data.Maybe (fromMaybe)

--Exercicio 1

type MSet a = [(a,Int)]

--a)

cardMSet :: MSet a -> Int
cardMSet l = sum $ map snd l

--b)

moda :: MSet a -> [a]
moda [] = []
moda l@((x,e):ls) | e == max = x:moda ls  
                  | otherwise = moda ls
    where
        max = maximum (map snd l)

--c)

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,e):ls) = replicate e x ++ converteMSet ls

--d)

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies l@((b,e):ls) x n | elemMSet l x = addMSet l x n
                            | e > n = (b,e):addNcopies ls x n
                            | otherwise = (x,n):l

elemMSet :: Eq a=> MSet a -> a -> Bool 
elemMSet [] _ = False 
elemMSet ((x,_):ls) n = x == n || elemMSet ls n

addMSet :: Eq a => MSet a -> a -> Int -> MSet a
addMSet [] x n = [(x,n)]
addMSet ((b,e):ls) x n | b == x = (b,e+n):ls
                       | otherwise = (b,e):addMSet ls x n

--Exercicio 2

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

--a)

instance Show SReais where
    show (AA e d) = "]" ++ show e ++ show d ++ "["
    show (AF e d) = "]" ++ show e ++ show d ++ "]"
    show (FF e d) = "[" ++ show e ++ show d ++ "]"
    show (FA e d) = "[" ++ show e ++ show d ++ "["
    show (Uniao e d) = "( " ++ show e ++ "U" ++ show d ++ " )"

--b)

pertence :: Double-> SReais -> Bool
pertence x (AA e d) | x > e && x < d = True 
                    | otherwise = False 
pertence x (AF e d) | x > e && x <= d = True 
                    | otherwise = False 
pertence x (FA e d) | x >= e && x < d = True 
                    | otherwise = False 
pertence x (FF e d) | x >= e && x <= d = True 
                    | otherwise = False 
pertence x (Uniao e d) = pertence x e || pertence x d

--c)

tira :: Double -> SReais -> SReais
tira x (AA e d) | pertence x (AA e d) = Uniao (AA e x) (AA x d)
                | otherwise = AA e d
tira x (FA e d) | x == e = AA e d
                | pertence x (FA e d) = Uniao (FA e x) (AA x d)
                | otherwise = FA e d
tira x (AF e d) | x == d = AA e d
                | pertence x (AF e d) = Uniao (AA e x) (AF x d)
                | otherwise = AF e d
tira x (FF e d) | x == e = AF e d
                | x == d = FA e d
                | pertence x (FF e d) = Uniao (FA e x) (AF x d)
                | otherwise = FF e d
tira x (Uniao e d) = Uniao (tira x e) (tira x d)

--Exercicio 3

data RTree a = R a [RTree a]

--a)

percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre _ (R a []) = Nothing
percorre (h:t) (R a r) | length r < h || null auxX = Nothing
                       | otherwise = Just (a:auxX)
    where aux = percorre t (r !! (h - 1))
          auxX = fromMaybe [] aux

--b)

procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a r) | n == a = Just []
                  | null r = Nothing
                  | otherwise = foldl (\acc num -> if procura n (r !! (num - 1)) == Nothing then acc else Just (num:fromMaybe [] (procura n (r !! (num - 1))))) Nothing [1..length r]