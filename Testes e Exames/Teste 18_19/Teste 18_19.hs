--Teste 18_19

import Data.Char
import System.Random

--Exercicio 1

--a)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n l = elemIndicesAcc 0 n l
        where
            elemIndicesAcc _ _ [] = []
            elemIndicesAcc p n (l:ls) | l == n = p:elemIndicesAcc (p+1) n ls
                                      | otherwise = elemIndicesAcc (p+1) n ls 

--b)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True 
isSubsequenceOf _ [] = False 
isSubsequenceOf t@(x:xs) (l:ls) | x == l = isSubsequenceOf xs ls
                                | otherwise = isSubsequenceOf t ls

--Exercicio 2

data BTree a = Empty | Node a (BTree a) (BTree a)

--a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node (a,b) e d) | x == a = Just b
                            | x < a = lookupAP x e
                            | x > a = lookupAP x d

--b)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty t = Empty
zipWithBT f t Empty = Empty
zipWithBT f (Node a e d) (Node as es ds) = Node (f a as) (zipWithBT f e es) (zipWithBT f d ds)

--Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha l = digitAlphaAcc ([],[]) l
        where
            digitAlphaAcc (d,c) [] = (d,c)
            digitAlphaAcc (d,c) (l:ls) | isDigit l = digitAlphaAcc (d++[l],c) ls
                                       | isAlpha l = digitAlphaAcc (d,c++[l]) ls 
                                       | otherwise = digitAlphaAcc (d,c) ls 

--Exercicio 4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
    

--a)

firstSeq :: Seq a -> a
firstSeq (Cons x Nil) = x
firstSeq (App Nil y)  = firstSeq y
firstSeq (App x y)    = firstSeq x

--b)

dropSeq :: Int -> Seq a -> Seq a
dropSeq  _ Nil = Nil
dropSeq n (Cons a s) | n > nss = Nil
                     | n == nss = Cons a Nil
                     | otherwise = Cons a (dropSeq n s)
           where
              nss = contaCons s
dropSeq n (App s1 s2) | n > ns = dropSeq (n-ns) s2
                      | n == ns = s2
                      | otherwise = App (dropSeq n s1) s2
           where
              ns = contaCons s1
              

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons a s) = 1+ contaCons s
contaCons (App s1 s2) = contaCons s1 + contaCons s2

--c)

instance Show a => Show (Seq a) where
    show  n = "<< " ++ mostra n ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ ", " ++ mostra s
mostra (App e d) = mostra e ++ ", " ++ mostra d

--Exercicio 5

type Mat a = [[a]]

--a)

getElem :: Mat a -> IO a
getElem m = do 
            let altura = length m
                largura = length (head m)
            y <- randomRIO(0,altura-1)
            x <- randomRIO(0,largura-1)
            return (m !! y !! x)

--b)

magic :: Mat Int -> Bool
magic m = linhasOk && colunasOk && diagonaisOk
    where linhasOk = verificaLinhas m numeroMagico
          colunasOk = verificaColunas m numeroMagico
          diagonaisOk = diagonal1 m == numeroMagico && diagonal2 m == numeroMagico
          numeroMagico = sum (head m)

verificaLinhas :: Mat Int -> Int -> Bool
verificaLinhas [] _ = True
verificaLinhas (l:ls) n = (sum l == n) && verificaLinhas ls n

verificaColunas :: Mat Int -> Int -> Bool
verificaColunas ([]:xs) _ = True 
verificaColunas m n = sum coluna == n && verificaColunas resto n
    where
        coluna = map head m
        resto = map tail m 

diagonal1 :: Mat Int -> Int
diagonal1 m = sum [m !! i !! i | i <- [0..tamanho-1]]
    where
        tamanho = length m

diagonal2 :: Mat Int -> Int
diagonal2 m = sum [m !! i !! j | i <- [0..tamanho-1], let j = (tamanho-1)-i]
        where 
            tamanho = length m