
--MiniTeste 1 2012/2013

import Data.Char

-- Exercicio 1

--a)

nome :: String -> Bool
nome (x:y:xs) = isLower y

--b)

type Jogo = (String, Int, String, Int)

equipaComXgolos :: Jogo -> Int -> String
equipaComXgolos (e1,g1,e2,g2) g | g1 == g = e1
                                | g2 == g = e2
                                | otherwise = []

--c)

jogosComXgolos :: [Jogo] -> Int -> Int
jogosComXgolos ((e1,g1,e2,g2):xs) g | g == g1 && g == g2 = 2 + jogosComXgolos xs g
                                    | g == g1 || g == g2 = 1 + jogosComXgolos xs g
                                    | otherwise = jogosComXgolos xs g

--Exercicio 2

type Ponto = (Float, Float)
type Rectangulo = (Ponto, Ponto)

quadrado :: Rectangulo -> Bool
quadrado ((x,y),(xs,ys)) | abs(x - xs) == abs(y -ys) = True
                         | otherwise = False

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (x:xs) | quadrado x = 1+contaQuadrados xs
                      | otherwise = contaQuadrados xs

--Exercicio 3

--a)

dist' :: (Float,Float) -> Float
dist' (x,y) = sqrt(d1 + d2)
    where
        d1 = (x - 0)^2
        d2 = (x - 0)^2

--b)

fun :: [Float] -> [Float]
fun [] = []
fun (h:t) = if h>=0 then h : (fun t) 
            else (fun t)

{-
fun [3, -5, 0, -3, 2]
3 : fun [-5,0,-3,2]
3 : fun [0,-3,2]
3 : 0 : fun [-3,2]
3 : 0 : fun [2]
3 : 0 : 2 : fun []
3 : 0 : 2 : []
[3,0,2]
-}

--c)

somaNeg :: [Int] -> Int 
somaNeg [] = 0
somaNeg (x:xs) | x < 0 = x + somaNeg xs
               | otherwise = somaNeg xs

--Exercicio 4

escala :: Float -> Rectangulo -> Rectangulo
escala n ((x,y),(xs,ys)) = ((x,y),(n*(xs-x)+x,n*(ys-y)+y))

escalaTudo :: Float -> [Rectangulo] -> [Rectangulo]
escalaTudo n l = map (escala n) l

--Exercicio 5

--a)

mults :: Int -> Int -> Int -> Bool
mults x y z | mod x y == 0 && mod x z == 0 = True
            | mod y x == 0 && mod y z == 0 = True
            | mod z x == 0 && mod z y == 0 = True
            | otherwise = False

--b)

fun5 :: [Float] -> Float
fun5 [] = 1
fun5 (x:xs) = x * (fun5 xs)

{-
fun5 [3,5,2]
=> 3*fun5[5,2]
=> 3*5*fun5[2]
=> 3*5*2*fun5[]
=> 3*5*2*1
=> 30
-}

--c)

triplos :: [Int] -> [Int]
triplos l = map (3*) l

--Exercicio 6

--a)

p :: Int -> Bool
p 0 = True
p 1 = False
p x | x > 1 = p (x-2)

{- 
p 5
=> p 3
=> p 1
=> False
-}

--b)

type Circulo = (Ponto, Float)

--I)

dist :: (Float,Float) -> (Float, Float) -> Float
dist (x,y) (xs,ys)= sqrt(d1 + d2)
    where
        d1 = (x - xs)^2
        d2 = (y - ys)^2

dentro :: Ponto -> Circulo -> Bool
dentro p (p0,r) | (dist p p0) < r = True
                | otherwise = False

--II)

filtra :: Ponto -> [Circulo] -> Int
filtra _ [] = 0
filtra x (c:cs) | dentro x c = 1+filtra x cs
                | otherwise = filtra x cs

--Exercicio 7

--a)

somaIgual :: Int -> Int -> Int -> Bool
somaIgual x y z = (x+y) == z || (x+z) == y || (y+z) == x

--b)

resJogo :: Jogo -> Char
resJogo (_,g1,_,g2) | g1 == g2 = 'x'
                    | g1 < g2 = '2'
                    | otherwise = '1'

--c)

vicVist :: [Jogo] -> Int
vicVist [] = 0
vicVist (j:js) | resJogo j == '2' = 1+ vicVist js
               | otherwise = vicVist js

--Exercicio 8

--a)

supSoma :: Int -> Int -> Int -> Bool
supSoma x y z = (x+y) < z || (x+z) < y || (y+z) < x

--b)

fun8 :: [Float] -> Float
fun8 [] = 0
fun8 (y:ys) = y^2 + (fun8 ys)

{-
fun8 [2,3,5]
=> 2^2 + fun8[3,5]
=> 4 + 3^2 + fun8[5]
=> 4 + 9 + 5^2 + fun8[]
=> 4 + 9 + 25 + 0
=> 38
-}

--c)

soDigitos :: [Char] -> [Char]
soDigitos l = filter isDigit l

--Exercicio 9

area :: Rectangulo -> Float
area ((x,y),(xs,ys)) = abs (x-xs) * abs (y-ys)

areaTotal :: [Rectangulo] -> Float
areaTotal l = sum $ map area l

--Exercicio 10

--a)

p' :: Int -> Bool
p' 0 = True
p' 1 = False
p' x | x > 1 = p' (x-2)

{- 
p 5
=> p 3
=> p 1
=> False
-}

--b)

--I)

fora :: Ponto -> Circulo -> Bool
fora p (c,r) = (dist p c) > r

--II)

filtra2 :: Circulo -> [Ponto] -> Int
filtra2 c l = length $ filter (\x -> fora x c) l

--Exercicio 11

--a)

mults' :: Int -> Int -> Int -> Bool
mults' x y z | mod x y == 0 && mod x z == 0 = True
             | mod y x == 0 && mod y z == 0 = True
             | mod z x == 0 && mod z y == 0 = True
             | otherwise = False

--b)

fun'' :: [Int] -> [Int]
fun'' [] = []
fun'' (h:t) = if (mod h 2)==0 then h : (fun'' t)
                              else (fun'' t)

{- 
fun'' [8,5,12,7]
=> 8 : fun''[5,12,7]
=> 8 : fun''[12,7]
=> 8 : 12 : fun''[7]
=> 8 : 12 : fun''[]
=> 8 : 12 : []
=> [8,12]
-}

--c)

minusculas :: [Char] -> Int
minusculas c = length $ filter isLower c

--Exercicio 12

--a)

nome' :: [Char] -> Bool
nome' (c:cs) = isUpper c

--b)

golosEquipa :: Jogo -> String -> Int
golosEquipa (e1,g1,e2,g2) e | e == e1 = g1
                            | e == e2 = g2
                            | otherwise = -1

--c)

golos :: [Jogo] -> String -> Int
golos l e = sum $ map (\x -> golosEquipa x e) l

--Exercicio 13

roda :: Rectangulo -> Rectangulo
roda ((x1,y1),(x2,y2)) = ((x1,y1),(x1 + (y2-y1), y1 + (x2-x1)))

rodaTudo :: [Rectangulo] -> [Rectangulo]
rodaTudo l = map (\x -> roda x) l

--Exercicio 14

--a)

maior :: Int -> Int -> Int -> Bool
maior x y z = (x+y) < z || (x+z) < y || (y+z) < x

--b)

resJogo' :: Jogo -> String
resJogo' (e1,g1,e2,g2) | g1 > g2 = "Ganhou equipa da casa"
                       | g1 < g2 = "Ganhou equipa de fora"
                       | otherwise = "Empate"

--c)

empate :: [Jogo] -> Int
empate l = length $ filter (\x -> (resJogo' x) == "Empate") l