--Epoca Especial 18_19

--Exercicio 1

--a)

isSorted :: (Ord a) => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True 
isSorted (x:y:z) = x <= y && isSorted (y:z)

--b)

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits(init1 l) ++ [l]

init1 :: [a] -> [a]
init1 [x] = []
init1 (x:xs) = x:init1 xs

--Exercicio 2

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB [x] = x
maximumMB ((Just x):(Just y):xs) | x < y = maximumMB (Just y:xs)
                                 | otherwise = maximumMB (Just x:xs)
maximumMB ((Just x): Nothing :xs)= maximumMB (Just x:xs)
maximumMB (Nothing:(Just x):xs)  = maximumMB (Just x:xs)
maximumMB (Nothing:Nothing:xs)   = maximumMB (Nothing:xs)

--Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a)

--a)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

--b)

instance Show a => Show (LTree a) where
        show lt = unlines $ map (\(c,n) -> replicate n '.' ++ show c) (travessia lt)

travessia :: LTree a -> [(a,Int)]
travessia (Tip c) = [(c,0)]
travessia (Fork e d) = map(\(n,e) -> (n,e+1)) (travessia e) ++ travessia d

--Exercicio 4

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInitAcc (sum l) (inits l)
    where
        maxSumInitAcc n [] = n
        maxSumInitAcc n (x:xs) | n > sum x = maxSumInitAcc n xs
                               | otherwise   = maxSumInitAcc (sum x) xs

--Exercicio 5

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

--a)

convPL :: (Eq a) => RelP a -> RelL a
convPL l = map (\x -> (fst(head x),juntarPares x)) (filterPares l)

filterPares :: (Eq a) =>  RelP a -> [[(a,a)]]
filterPares [] = []
filterPares [x] = [[x]]
filterPares l@((x,y):ls) = filter (\(a,b) -> a == x)  l : filterPares (filterParesAux x l)
                            

filterParesAux :: (Eq a) => a -> [(a,a)] -> [(a,a)]
filterParesAux x  = filter (\n-> fst n /= x) 

juntarPares :: [(a,a)] -> [a]
juntarPares  = map snd 

--b)

criaRelPint :: Int -> IO (RelP Int)
criaRelPint n = do
            putStrLn "Insira um par um valor de cada vez"
            c <- getLine 
            d <- getLine 
            let a = read c :: Int
                b = read d :: Int
                l = insiraLista (a,b) []
            geraRelP l (n-1)


geraRelP :: [(Int,Int)] -> Int -> IO (RelP Int) 
geraRelP l n = do
            if n /= 0 then do
                putStrLn "Insira um par um valor de cada vez"
                c <- getLine 
                d <- getLine 
                let a = read c :: Int
                    b = read d :: Int
                    r = insiraLista (a,b) l
                geraRelP r (n-1)
            else
                return l

insiraLista :: (a,a) -> [(a,a)] -> [(a,a)]
insiraLista (x,y) [] = [(x,y)]
insiraLista (x,y) (l:ls) = l:insiraLista (x,y) ls

--c)

--I)

convFP :: (Eq a) => RelF a -> RelP a
convFP l = concat $ map (\x -> (map (\l -> (x,l)) ((snd l) x))) (fst l)

--II)

convPF :: (Eq a) => RelP a -> RelF a
convPF x = (map fst y,f)
    where y = convPL x
          f a = foldl (\acc (b,c) -> if a == b then c else acc) [] y
