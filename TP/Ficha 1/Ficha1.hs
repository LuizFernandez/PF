
-- Ficha 1

import Data.Char

-- Exercicio 1

 -- a)

perimetro' :: Float -> Float
perimetro' r = 2*pi*r

-- b)

type Ponto1 = (Double,Double)

dist' :: Ponto1 -> Ponto1 -> Double
dist' (x1,y1) (x2,y2) = sqrt(d1+d2)
        where
            d1 = (x1-x2)^2
            d2 = (y1-y2)^2

-- c)

primUlt :: [Int] -> (Int,Int)
primUlt l = (head l,last l)

-- d)

multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0 then True else False 

-- e)

truncaImpar :: [Int] -> [Int]
truncaImpar l = if odd (length l) then tail l else l

-- f)

max2 :: Int -> Int -> Int
max2 x y = if x>y then x else y

max3 :: Int -> Int -> Int -> Int 
max3 x y z = max2(max2 x y)z

-- Exercicio 2

-- a)

nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c = if det<0 then 0 else if det==0 then 1 else 2
    where
        det = b^2 - 4*a*c

-- b)

raizes :: Double -> Double -> Double -> [Double]
raizes a b c = if n==0 then [] else if n==1 then [r1] else [r1,r2]
        where
            det = b^2 - 4*a*c
            r1 = (-b + sqrt det) / 2*a
            r2 = (-b - sqrt det) / 2*a
            n = nRaizes a b c

-- Exercicio 3
 
type Hora = (Int,Int)

--a)

horaValida :: Hora -> Bool
horaValida (h,m) = if 0 <= h && h < 24 && 0 <= m && m < 60 then True else False 

--b)

aDepois :: Hora -> Hora -> Bool
aDepois (h1,m1) (h2,m2) = if h1>h2 then True else if h1<h2 then False else if h1==h2 && m1>m2 then True else False

--c)

converterMin :: Hora -> Int
converterMin (h,m) = h*60 + m

--d)

converterHor :: Int -> Hora
converterHor m = ((div m 60),(mod m 60))

--e)

dHoras:: Hora -> Hora -> Int
dHoras (h1,m1) (h2,m2) = abs(converterMin (h1,m1)-converterMin(h2,m2))

--f)

adicMin :: Hora -> Int -> Hora
adicMin (h1,m1) m2 = converterHor(m2+converterMin(h1,m1))

--Exercicio 4

data Hora1 = H Int Int
        deriving (Show,Eq)

--a)

horaValida2 :: Hora1 -> Bool
horaValida2 (H h m) = if 0<=h && h<24 && 0<=m && m<60 then True else False

--b)

aDepois2 :: Hora1 -> Hora1 -> Bool
aDepois2 (H h1 m1) (H h2 m2) = if h1>h2 then True else if h1<h2 then False else if h1==h2 && m1>m2 then True else False

--c) 

converteMin2 :: Hora1 -> Int
converteMin2 (H h m) = h*60+m

--d)

converteHor2 :: Int -> Hora1
converteHor2 m = (H (div m 60) (mod m 60))

--e)

dHoras2 :: Hora1 -> Hora1 -> Int
dHoras2 (H h1 m1) (H h2 m2) = abs(converteMin2(H h1 m1) - converteMin2(H h2 m2))

--f)

adicMinutos2 :: Hora1 -> Int -> Hora1 
adicMinutos2 (H h m) m1 = (converteHor2(converteMin2(H h m)+m1))

--Exercicio 5

data Semaforo = Verde|Amarelo|Vermelho
            deriving(Show,Eq)

--a)

next :: Semaforo -> Semaforo
next s = case s of
        Verde    -> Amarelo
        Vermelho -> Verde
        Amarelo  -> Vermelho

--b)

stop :: Semaforo -> Bool
stop s | s == Vermelho = True
       | otherwise = False

--c)

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double
                deriving (Show,Eq)

--a) 

posx :: Ponto -> Double 
posx (Cartesiano x y) = x
posx p1               = posx (polar2cart p1)

polar2cart :: Ponto -> Ponto
polar2cart c@(Cartesiano x y) = c
polar2cart (Polar r a)        = Cartesiano (r*sin a) (r*cos a)

--b)

posy :: Ponto -> Double 
posy (Cartesiano x y) = y
posy p1               = posy (polar2cart p1)

--c) 

raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt(x^2+y^2)
raio (Polar r a)      = r

--d) 

angulo      :: Ponto -> Double 
angulo p = case p of 
          Cartesiano x y -> if x < 0 && y == 0 then pi 
                                               else if x < 0 then pi + atan (y/x) 
                                                             else atan (y/x)
          Polar r a      -> a 

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180
 
toDegrees   :: Floating a => a -> a
toDegrees rad = rad * 180 / pi
 
--e)

dist :: Ponto -> Ponto -> Double 
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt(d1+d2)
        where
            d1 = (x1-x2)^2
            d2 = (y1-y2)^2
dist p1 p2                                 = dist (polar2cart p1) (polar2cart p2)

--Exercicio 7

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

--a)

poligono :: Figura -> Bool
poligono (Circulo p1 a)       = a > 0
poligono (Rectangulo p1 p2)   = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2)

--b)

vertices :: Figura -> [Ponto]
vertices (Circulo p1 a) = []
vertices r@(Rectangulo p1 p2) | poligono r = [(p1),(Cartesiano (posx p1) (posy p2)),(Cartesiano (posx p2) (posy p1)),(p2)]
                              | otherwise = []
vertices t@(Triangulo p1 p2 p3) | poligono t = [p1,p2,p3]
                                | otherwise = []

--c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist p1 p2
                                b = dist p2 p3
                                c = dist p3 p1
                                s = (a+b+c)/2
                            in sqrt(s*(s-a)*(s-b)*(s-c))
area (Circulo p1 r) = (r^2) * pi
area (Rectangulo p1 p2) = a*b
                    where
                       a = abs((posx p1) - (posx p2))
                       b = abs((posy p1) - (posy p2))

--d)

perimetro :: Figura -> Double
perimetro (Circulo p1 r) = r*2*pi
perimetro (Triangulo p1 p2 p3) = a+b+c
                                where
                                a = dist p1 p2
                                b = dist p2 p3
                                c = dist p3 p1
perimetro (Rectangulo p1 p2) = 2 * abs (posx p2 - posx p1) + 2*abs(posy p2 - posy p1)

--Exercicio 8

--a)

isLower :: Char -> Bool
isLower x | ord x >= 97 && ord x <= 122 = True
          | otherwise = False

--b) 

isDigit :: Char -> Bool
isDigit x | x >= '1' && x <= '9' = True
          | otherwise = False

--c)

isAlpha :: Char -> Bool
isAlpha x | x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' = True
          | otherwise = False

--d)

toUpper2 :: Char -> Char
toUpper2 x | x >= 'a' && x <= 'z' = chr((ord x)-32)
           | otherwise = x

--e)

intToDigit :: Int -> Char
intToDigit x = chr (x + 48)

--f) 

digitToInt :: Char -> Int
digitToInt x = ord x - 48
