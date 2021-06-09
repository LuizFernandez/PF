--Teste 17_18

import System.Random

--Exercicio 1

insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (l:ls) | x > l     = l:insert1 x ls
                 | otherwise = x:l:ls

--Exercicio 2

catMaybes :: [Maybe a] -> [a]
catMaybes l = [x | Just x <- l]

--Exercicio 3

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const x)  = show x
    show (Var s)    = s
    show (Mais e x) = "(" ++ show e ++ " + " ++ show x ++ ")"
    show (Mult e x) = "(" ++ show e ++ " * " ++ show x ++ ")"

--Exercicio 4

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insertOn f x (sortOn f xs)

insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f x [] = [x]
insertOn f x (l:ls) | f x < f l = x:l:ls    
                    | otherwise = l:insertOn f x ls

--Exercicio 5

--a)

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = max - min     
        where
            (max,min) = maxMin l (head l,head l)

maxMin :: [Int] -> (Int,Int) -> (Int,Int)
maxMin [] (x,y) = (x,y)
maxMin (l:ls) (x,y) | l <= y = maxMin ls (x,l)
                    | l >  x = maxMin ls (l,y)
                    | otherwise = maxMin ls (x,y)

--b)

parte :: [Int] -> ([Int],[Int])
parte l = foldl (\(acc1,acc2) (a,b) -> if amplitude acc1 + amplitude acc2 < amplitude a + amplitude b then (acc1,acc2) else (a,b)) (l,l) combinacoes
       where
         combinacoes = foldl (\acc n -> splitAt n sl : acc) [] [1..(length l -1)]
         sl = sort l

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert2 x (sort xs)

insert2 :: Ord a =>  a -> [a] -> [a]
insert2 x [] = [x]
insert2 x (l:ls) | x > l = l:insert2 x ls
                 | otherwise = x:l:ls

--Exercicio 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

--a)

conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover (_,_) i) = conta i
conta (Juntar l)      = sum $ map conta l

--b)

apaga :: Imagem -> IO Imagem
apaga i = do
       let ci = indicesquadrados i
       ri <- randomRIO(0,length ci)
       let takei = ci !! (ri -1)
       return $ apagaquadrado takei i

indicesquadrados :: Imagem -> [Int]
indicesquadrados (Quadrado x) = [x]
indicesquadrados (Mover (_,_) i) = indicesquadrados i
indicesquadrados (Juntar l) = concat $ map indicesquadrados l

apagaquadrado :: Int -> Imagem -> Imagem
apagaquadrado n (Quadrado x) | n == x = Juntar []
                             | otherwise = Quadrado x
apagaquadrado n (Mover (x,y) i) = Mover (x,y) (apagaquadrado i)
apagaquadrado n (Juntar l) = Juntar map (apagaquadrado n) l
