
--Ficha4

import Data.Char
import Data.List

--Exercicio 1

--a)

equaA :: [Int]
equaA = [6*x | x <- [1..3]]

--b)

equaB :: [Int]
equaB = [6*x | x <- [1..3]]

--c)
umc :: [Int]->[Int]->[(Int,Int)]
umc [] ys = []
umc (x:xs) ys = junta x ys ++ umc xs ys

junta :: Int->[Int]->[(Int,Int)]
junta x [] = []
junta x (y:ys) = (x,y) : junta x ys

--d)

listad2 :: [Int]
listad2 = map (\x -> sum (filter odd [1..x])) [1..10]


--Exercicio 2

--a)

equa2A :: [Int]
equa2A = [2^y | y <- [0..10]]

--b)

equa2B :: [(Int,Int)]
equa2B = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

--c)

equa2C :: [[Int]]
equa2C = [[y | y <- [1..x]] | x <- [1..5]]

--d)

equa2D :: [[Int]]
equa2D = [replicate x 1 | x <- [1..5]]

--e)

equa2E :: [Int]
equa2E = [product [y | y <- [1..x]] | x <- [1..6]]

--Exercicio 3

digitAlpha :: String->(String,String)
digitAlpha s = (digits s,alphas s)

digits :: String->String
digits [] = []
digits (c:cs) | isDigit c = c:digits cs
              | otherwise = digits cs

alphas :: String->String
alphas [] = []
alphas (c:cs) | isAlpha c = c:alphas cs
              | otherwise = alphas cs

digitAlpha2 :: String -> (String,String)
digitAlpha2 []= ([],[])
digitAlpha2 (c:cs)| isDigit c = (c:ds,as)
                  | isAlpha c = (ds,c:as)
                  | otherwise = (ds,as)
        where 
            (ds,as)=digitAlpha2 cs

digitAlpha3 :: String->(String,String)
digitAlpha3 s = digitAlpha3Acc ([],[]) s

digitAlpha3Acc :: (String,String)->String->(String,String)
digitAlpha3Acc (ds,as) [] = ([],[])
digitAlpha3Acc (ds,as) (c:cs) | isDigit c = digitAlpha3Acc (ds++[c],as) cs
                              | isAlpha c = digitAlpha3Acc (ds,as++[c]) cs
                              | otherwise = digitAlpha3Acc (ds,as) cs

--Exercicio 4

nzp :: [Int] -> (Int,Int,Int)
nzp l = nzpAcc (0,0,0) l

nzpAcc :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpAcc (a,b,c) [] = (a,b,c)
nzpAcc (a,b,c) (h:hs) | h<0 = nzpAcc (a+1,b,c) hs
                      | h==0 = nzpAcc (a,b+1,c) hs
                      | h>0 = nzpAcc (a,b,c+1) hs

--Exercicio 5

divMod2 :: Integral a => a -> a -> (a, a)
divMod2 c d = divModAcc (0,0) c d

divModAcc :: Integral a => (a,a) -> a -> a -> (a,a)
divModAcc (a,b) c d | c>d = divModAcc (a+1,b) (c-d) d
                    | c==d = (a+1,0)
                    | c<d = (a,b+c)

--Exercicio 6

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits2 :: [Int] -> Int
fromDigits2 l = fromDigits2Acc 0 l

fromDigits2Acc :: Int -> [Int] -> Int
fromDigits2Acc a [] = 0
fromDigits2Acc a (h:t) = h*10^(length t) + fromDigits2Acc a t

--Exercicio 7

maxSumInt :: (Num a, Ord a) => [a] -> a
maxSumInt l = maxSumIntAcc 0 l

maxSumIntAcc :: (Num a, Ord a) => a -> [a] -> a
maxSumIntAcc _ [] = 0
maxSumIntAcc n (x:xs) = x + (maxSumIntAcc n xs)

--Exercicio 8

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> (Int , Int)
fib' 0 = (0,1)
fib' 1 = (1,1)
fib' n = (x,y)
            where
                x = fib n
                y = fib (n+1)