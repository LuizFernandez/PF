
--50 Questoes V.2

import Data.Char
import Data.Either

--Exercicio 1

enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 x y | x==y = [x]
                | x>y = []
                | otherwise = x:enumFromTo (x+1) y

--Exercicio 2

enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 x y z | x==y && y==z = repeat x
                      | x==z = [x]
                      | x<y && y<z = x:enumFromThenTo2 y (y+(y-x)) z
                      | otherwise = []

--Exercicio 3

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] l = []
myPlusPlus l [] = []
myPlusPlus (x:xs) l = x:myPlusPlus xs l

--Exercicio 4

myExclamtion :: [a] -> Int -> a
myExclamtion (x:xs) n | n==0 = x
                      | n>0 = myExclamtion xs (n-1)

--Exercicio 5

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse xs ++ [x]

--Exercicio 6

take2 :: Int -> [a] -> [a]
take2 _ [] = []
take2 x (l:ls) | x<=0 = []
               | x>0 = l:(take2 (x-1) ls)

--Exercicio 7

drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 x t@(l:ls) | x<=0 = t
                 | x>0 = drop2 (x-1) ls

--Exercicio 8

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] l = []
zip2 l [] = []
zip2 (x:xs) (y:ys) = (x,y):zip2 xs ys

--Exercicio 9

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (l:ls) | x==l = True
               | otherwise = elem2 x ls

--Exercicio 10

replicate2 :: Int -> a -> [a]
replicate2 n x | n<=0 = []
               | otherwise = x:replicate2 (n-1) x

--Exercicio 11

intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 _ [x] = [x]
intersperse2 x (l:ls) = l:x:intersperse2 x ls

--Exercicio 12

group2 :: Eq a => [a] -> [[a]]
group2 [] = [[]]
group2 l = take3 (groupAcc l) l: group2 (drop3 (groupAcc l)l)

groupAcc :: Eq a => [a] -> Int
groupAcc [x] = 1
groupAcc (x:y:xs) | x==y = 1+groupAcc (y:xs)
                  | otherwise = 1

take3 :: Int -> [a] -> [a]
take3 _ [] = []
take3 x (l:ls) | x<=0 = []
               | x>0 = l:take3 (x-1) ls

drop3 :: Int -> [a] -> [a]
drop3 _ [] = []
drop3 x t@(l:ls) | x<=0 = t
                 | x>0 = drop3 (x-1) ls 

--Exercicio 13

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (l:ls) = l++concat2 ls

--Exercicio 14

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 l = inits2(init2 l) ++ [l]

init2 :: [a] -> [a]
init2 [x] = []
init2 (x:xs) = x:init2 xs

--Exercicio 15

tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 l = [l] ++ tails2(tail l)

tail3 :: [a] -> [a]
tail3 [] = []
tail3 (x:xs) = xs

--Exercicio 16

isPrefixOf2 :: Eq a => [a] -> [a] -> Bool
isPrefixOf2 [] l = True
isPrefixOf2 (x:xs) (l:ls) | x==l = isPrefixOf2 xs ls
                          | otherwise = False 

--Exercicio 17

isSuffixOf2 :: Eq a => [a] -> [a] -> Bool
isSuffixOf2 [] l = True
isSuffixOf2 x l | last x==last l = isSuffixOf2 (init x) (init l)
                | otherwise = False

last2 :: [a] -> a
last2 (x:[]) = x
last2 (x:xs) = last2 xs

init3 :: [a] -> [a]
init3 [x] = []
init3 (x:xs)= x:init xs

--Exercicio 18

isSubsequenceOf2 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf2 [] _ = True
isSubsequenceOf2 _ [] = False
isSubsequenceOf2 t@(x:xs) (l:ls) | x==l = isSubsequenceOf2 xs ls
                                 | otherwise = isSubsequenceOf2 t ls

--Exercicio 19 

elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 _ [] = []
elemIndices2 x l = elemIndices2Acc 0 x l
            where 
                elemIndices2Acc _ _ [] = []
                elemIndices2Acc n x (l:ls) | x==l = n:elemIndices2Acc (n+1) x ls
                                           | otherwise = elemIndices2Acc (n+1) x ls

--Exercicio 20

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (l:ls) = l:nub2 (nub2Acc l ls)
                where
                    nub2Acc l [] = []
                    nub2Acc l (x:xs) | l==x = nub2Acc l xs
                                     | otherwise = x:nub2Acc l xs

--Exercicio 21

delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] = []
delete2 x (l:ls) | x==l = ls
                 | otherwise = l:delete2 x ls

--Exercicio 22

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) (x:xs) l | elem2 x l = (\\) (delete2 x l) xs
              | otherwise = (\\) xs l

--Exercicio 23

union2 :: Eq a => [a] -> [a] -> [a]
union2 [] l = l
union2 t (l:ls) | elem3 l t  = union2 t ls
                | otherwise = union2 (t++[l]) ls 
                
elem3 :: Eq a => a -> [a] -> Bool
elem3 _ [] = False
elem3 x (l:ls) | l==x = True
               | otherwise = elem3 x ls

--Exercicio 24

intersect2 :: Eq a => [a] -> [a] -> [a]
intersect2 [] l = []
intersect2 (l:ls) t | elem4 l t = l:(intersect2 ls t)
                    | otherwise = intersect2 ls t

elem4 :: Eq a => a -> [a] -> Bool
elem4 _ [] = False
elem4 x (l:ls) | l==x = True
               | otherwise = elem4 x ls

--Exercicio 25

insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x t@(l:ls) | x>l = l:insert2 x ls
                   | otherwise = x:t
           
--Exercicio 26

unwords2 :: [String] -> String
unwords2 [] = ""
unwords2 [x] = x
unwords2 (l:ls) = l++" "++unwords2 ls

myPlusPlus2 :: [a] -> [a] -> [a]
myPlusPlus2 [] l = []
myPlusPlus2 l [] = []
myPlusPlus2 (x:xs) l= x:myPlusPlus2 xs l

--Exercicio 27

unlines2 :: [String] -> String
unlines2 [] = []
unlines2 (x:xs) = x++"\n"++unlines2 xs

myPlusPlus3 :: [a] -> [a] -> [a]
myPlusPlus3 [] l = []
myPlusPlus3 l [] = []
myPlusPlus3 (x:xs) l= x:myPlusPlus3 xs l

--Exercicio 28

pMaior :: Ord a =>  [a] -> Int 
pMaior (l:ls) = posicao (maior l ls) (l:ls)
                    where
                        maior l [] = l
                        maior l (x:xs) | l>x = maior l xs
                                       | otherwise = maior x xs
                        posicao l (x:xs) | l==x = 0
                                         | otherwise = 1+posicao l xs

--Exercicio 29

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [x] = False
temRepetidos (x:xs) | elem5 x xs == True = True
                    | otherwise = temRepetidos xs

elem5 :: Eq a => a -> [a] -> Bool
elem5 _ [] = False
elem5 x (y:ys) | x==y = True
               | otherwise = elem5 x ys
--Exercicio 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) | x>='0' && x<='9' = x:algarismos xs
                  | otherwise = algarismos xs

--Exercicio 31

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = [x]
posImpares (x:y:xs) = y:posImpares xs

--Exercicio 32

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = x:posPares xs

--Exercicio 33

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x<=y = isSorted (y:xs)
                  | otherwise = False
    
--Exercicio 34

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (l:ls) = insert3 l (iSort ls)

insert3 :: Ord a => a -> [a] -> [a]
insert3 x [] = [x]
insert3 x t@(l:ls) | x>l = l:insert3 x ls
                   | otherwise = x:t

--Exercicio 35

menor :: String -> String -> Bool 
menor [] l = False
menor l [] = True
menor (x:xs) (l:ls) | x<l = True
                    | x==l = menor xs ls
                    | otherwise = False
            
--Exercicio 36

elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _ [] = False
elemMSet x ((y,ys):ls) | x==y = True
                       | otherwise = elemMSet x ls

--Exercicio 37

lengthMSet :: [(a,Int)] -> Int 
lengthMSet [] = 0
lengthMSet ((y,ys):ls) = ys+lengthMSet ls

--Exercicio 38

converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((y,ys):ls) = replicate3 ys y ++ converteMSet ls

replicate3 :: Int -> a -> [a]
replicate3 0 _ = []
replicate3 n x = x:replicate3 (n-1) x

myPlusPlus4 :: [a] -> [a] -> [a]
myPlusPlus4 [] l = []
myPlusPlus4 l [] = []
myPlusPlus4 (x:xs) l = x:myPlusPlus4 xs l

--Exercicio 39 

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet x ((y,ys):ls) | x==y = (y,ys+1):ls
                         | otherwise = (y,ys):insereMSet x ls
                        
--Exercicio 40

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]  
removeMSet _ [] = []
removeMSet x ((l,ls):y) | x==l && ls==1 = y
                        | x==l = (l,ls-1):y
                        | otherwise = (l,ls):removeMSet x y

--Exercicio 41

constroiMSet :: Ord a => [a] -> [(a,Int)] 
constroiMSet [] = []
constroiMSet l = constroiMSetAcc l []
                where
                    constroiMSetAcc [] x = x
                    constroiMSetAcc (l:ls) x = constroiMSetAcc ls (insereMSet2 l x)

insereMSet2 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet2 _ [] = []
insereMSet2 x ((y,ys):l) | x==y = ((y,ys+1):l)
                         | otherwise = (y,ys):insereMSet2 x l

--Exercicio 42

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers l = let a=lefts2 l
                         b=rights2 l
                     in (a,b)

lefts2 :: [Either a b] -> [a]
lefts2 [] = []
lefts2 (Left a:l) = a:lefts2 l
lefts2 (Right a:l) = lefts2 l

rights2 :: [Either a b] -> [b]
rights2 [] = []
rights2 (Left b:l) = rights2 l
rights2 (Right b:l) = b:rights2 l

--Exercicio 43

catMaybes :: [Maybe a] -> [a]
catMaybes l = [x | Just x <- l]

--Exercicio 44

data Movimento = Norte | Sul | Este | Oeste
       deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (l:ls) = case l of 
                        Norte -> posicao (x,y+1) ls
                        Sul -> posicao (x,y-1) ls
                        Este -> posicao (x+1,y) ls
                        Oeste -> posicao (x-1,y) ls

--Exercicio 45

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xs,ys) | x==xs && y==ys = []
                      | y<ys = Norte:caminho (x,y+1) (xs,ys)
                      | y>ys = Sul: caminho (x,y-1) (xs,ys)
                      | x>xs = Oeste:caminho (x-1,y) (xs,ys)
                      | x<xs = Este:caminho (x+1,y) (xs,ys)

--Exercicio 46

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (x:xs) = case x of 
                    Norte -> vertical xs
                    Sul -> vertical xs
                    Oeste -> False
                    Este -> False

--Exercicio 47

data Posicao = Pos Int Int
        deriving Show
        
maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:z) = maisCentral ((central x y):z) 
            where
                central (Pos x y) (Pos xs ys) | (x^2+y^2)<(xs^2+ys^2) = (Pos x y)
                                              | otherwise = Pos xs ys
                                      
--Exercicio 48

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos xs ys):l) | x==xs && (y==ys+1 || y==ys-1) = (Pos xs ys):vizinhos (Pos x y) l
                                   | y==ys && (x==xs+1 || x==xs-1) = (Pos xs ys):vizinhos (Pos x y) l
                                   | otherwise = vizinhos (Pos x y) l

--Exercicio 49

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x y):(Pos xs ys):l) | y==ys = mesmaOrdenada ((Pos xs ys):l)
                                        | otherwise = False

--Exercicio 50

data Semaforo = Verde | Amarelo | Vermelho
        deriving Show
        
interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l = (naoVermelho l) < 2
                where
                    naoVermelho [] = 0
                    naoVermelho (l:ls) = case l of
                                    Vermelho -> naoVermelho ls
                                    otherwise -> 1+naoVermelho ls

