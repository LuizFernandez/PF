--Teste 20_21

import Data.Maybe

--Exercicio 1

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) l (m:ms) = (\\) (delete1 m l) ms

delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 x (l:ls) | x == l = ls
                 | otherwise = l : delete1 x ls

--Exercicio 2

type MSet a = [(a,Int)]

--a)

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((c,i):ls) | x == c && i == 1 = ls
                        | x == c = (c,i-1):ls
                        | otherwise = (c,i):removeMSet x ls

--b)

calcula :: MSet a -> ([a],Int)
calcula = foldl (\(acc1,acc2) (c,i) -> (acc1++[c],acc2+i)) ([],0) 

--Exercicio 3

partes :: String -> Char -> [String]
partes l c = partesAcc l c []
        where
            partesAcc [] _ l | null l = []
                             | otherwise = [l]
            partesAcc (l:ls) c m | l /= c = partesAcc ls c (m++[l])
                                 | otherwise = m : partesAcc ls c []

--Exercicio 4

data BTree a = Empty | Node a (BTree a) (BTree a)
        
 

exemplo1 :: Num a => BTree a
exemplo1 = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))

--a)

minSmin :: Ord a => BTree a -> (a,BTree a)
--minSmin (Node e Empty Empty) = (e,Empty)
minSmin (Node e Empty l)     = (e,l) 
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) | x < e = Node e (remove x l) r
                      | x > e = Node e l (remove x r)
                      | otherwise = aux x (Node e l r)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin r

removeVer2 :: Ord a => a -> BTree a -> BTree a
removeVer2 x Empty = Empty
removeVer2 x r@(Node a Empty Empty) | x == a = Empty
                                    | otherwise = r
removeVer2 x r@(Node a Empty d) | x == a = d
                                | otherwise = r
removeVer2 x r@(Node a e Empty) | x == a = e    
                                | otherwise = r
removeVer2 x (Node a e d) | x < a = Node a (removeVer2 x e) d
                          | x > a = Node a e (removeVer2 x d)
                          | x == a = Node r e nd
                    where
                        (r,nd) = minSmin d

--b)

instance Show a => Show (BTree a) where
    show Empty = "*"
    show (Node x e d) = "(" ++ show e ++ " <- " ++ show x ++ " -> " ++ show d ++ ")" 

--Exericico 5

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (l:ls) = insertOn f l (sortOn f ls)

insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f x [] = [x]
insertOn f x (l:ls) | f x > f l = l:insertOn f x ls
                    | otherwise = x:l:ls

--Exercicio 6

data FileSystem = File Nome | Dir Nome [FileSystem]

type Nome = String

exemplo2 :: FileSystem
exemplo2 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]], Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"]]

--a)

fichs :: FileSystem -> [Nome]
fichs (Dir _ []) = []
fichs (File s) = [s]
fichs (Dir x l) = concat $ map fichs l

--b)

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (Dir _ []) l = Nothing 
dirFiles (File s) m = Nothing 
dirFiles (Dir x l) [m] | x == m = Just (filter (not.null) (map takeFile l))
                       | otherwise = Nothing 
dirFiles (Dir x (l:ls)) (m:ms) | x == m && isJust (dirFiles l ms) = dirFiles l ms
                               | otherwise = dirFiles (Dir x ls) (m:ms)

takeFile :: FileSystem -> Nome
takeFile (File s) = s
takeFile _ = []

--c)

listaFich :: FileSystem -> IO ()
listaFich d = do
        putStrLn "Insert path"
        c <- getLine 
        let sPath = read c :: String
            path  = unPath sPath []
        if validPath path [d] then do
                              let valFile = showContent path [d]
                              if null valFile then putStrLn "Sem conteudo"
                                              else putStrLn valFile
                              else do
                                  putStrLn "Não é uma diretoria"
                                  listaFich d

unPath :: String -> String -> [String]
unPath [] l = [l]
unPath (l:ls) m | l == '/' = m : unPath ls []
                | otherwise = unPath ls (m++[l])

validPath :: [String] -> [FileSystem] -> Bool 
validPath [] l = True 
validPath l [] = False 
validPath t ((File x):ls) = validPath t ls
validPath t@(m:ms) ((Dir x l):ls) | m == x = validPath ms l 
                                  | otherwise = validPath t ls

showContent :: [String] -> [FileSystem] -> String
showContent [] l = takecontent l
showContent t ((File x):ls) = showContent t ls
showContent t@(m:ms) (Dir x l:ls) | x == m = showContent ms l
                                  | otherwise = showContent t ls

takecontent :: [FileSystem] -> String
takecontent [] = []
takecontent (File x:ls) = "File: " ++ x ++ "\n" ++ takecontent ls
takecontent (Dir x l:ls) = "Dir: " ++ x ++ "\n" ++ takecontent ls


