--Mini-Teste 2 2012/2013

--Exercicio 1

type MSet a = [(a,Int)]

--a)

insere :: Eq a => a -> MSet a -> MSet a
insere x [] = [(x,1)]
insere x ((n,o):h)| x == n = ((n,o+1):h)
                  | otherwise = (n,o):insere x h

--b)

moda :: MSet a -> [a]
moda xs = map fst $ filter ((==maxim).snd) xs
   where 
       maxim = maximum $ map snd xs

--Exercicio 2

--a)

type Radar = [(Hora,Matricula,VelAutor,VelCond)]
type Hora = (Int,Int)
type Matricula = String -- matricula do carro em infraccao
type VelAutor = Float   -- velocidade autorizada
type VelCond = Float    -- velocidade do condutor

verRadar :: Radar -> Bool
verRadar [] = True
verRadar ((_,_,va,vc):h) | va > vc = verRadar h
                         | otherwise = False

--b)

totalExcesso :: Radar -> Float
totalExcesso [] = 0
totalExcesso ((_,_,va,vc):h) = vc - va + totalExcesso h

--c)

hora2min :: Hora -> Int
hora2min (h,m) = h*60 +m

tempoSemInfrações :: Radar -> Int
tempoSemInfrações [(h,_,_,_),(h2,_,_,_)] = hora2min h2 - hora2min h
tempoSemInfrações ((h,_,_,_):y@(h2,_,_,_):l) = max (hora2min h2 - hora2min h)  (tempoSemInfrações (y:l))

--Exercicio 3

type Inscritos = [(Nume,Nome,Curso,Ano)]
type Nume = Integer
type Nome = String
type Curso = String
type Ano = Integer

--a)

aluCA :: (Curso, Ano) -> Inscritos -> Int
aluCA _ [] = 0
aluCA (c,a) ((_,_,cs,as):h) | cs == c && a == as = 1 + aluCA (c,a) h
                            | otherwise = aluCA (c,a) h

--b)

quantos :: Curso -> [Nume] -> Inscritos -> Int
quantos _ _ [] = 0
quantos c l ((n,_,cs,_):h) | c == cs && elem n l = 1 + quantos c l h
                           | otherwise = quantos c l h

--c)

doAno :: Ano -> Inscritos -> [(Nume, Nome, Curso)]
doAno _ [] = []
doAno a ((nu,no,c,ano):h) | a == ano = (nu,no,c):doAno a h
                          | otherwise = doAno a h

--Exercicio 4

--a)

elimina :: Eq a => a -> MSet a -> MSet a
elimina _ [] = []
elimina c ((x,1):h) = if (c == x) then h else elimina c h
elimina c ((x,i):h) | c == x = (x,i-1):h
                    | otherwise = (x,i):elimina c h

--b)

ordena :: MSet a -> MSet a
ordena [] = []
ordena (x:xs) = insertMSet x (ordena xs)

insertMSet :: (a,Int) -> MSet a -> MSet a
insertMSet (x,i) [] = [(x,i)]
insertMSet (x,i) l@((y,is):ls) | i > is = (y,is):insertMSet (x,i) ls
                               | otherwise = (x,i):l

--Exercicio 5

type PlayList = [(Titulo,Interprete,Duracao)]
type Titulo = String
type Interprete = String
type Duracao = Int -- duraçao da música em segundos

--a)

total :: PlayList-> Int
total [] = 0
total ((_,_,t):ls) = t + total ls

--b)

temMusicas :: [Interprete] -> PlayList -> Bool
temMusicas (x:xs) l = elem x inter && temMusicas xs l
    where
        inter = map (\(_,x,_) -> x) l

--c)

maior :: PlayList-> (Titulo, Duracao)
maior [(y,i,d)] = (y,d)
maior ((t,i,d):(ts,is,ds):ls) | d > ds = maior ((t,i,d):ls)
                              | otherwise = maior ((ts,is,ds):ls)

--Exercicio 6

--a)

excessoVelPorMatricula :: Matricula -> Radar -> Float
excessoVelPorMatricula _ [] = 0
excessoVelPorMatricula s ((_,m,va,vc):xs) | s == m = (vc-va) + excessoVelPorMatricula s xs
                                          | otherwise = excessoVelPorMatricula s xs

--b)

infracaoPorHora :: Int -> Radar -> Int
infracaoPorHora _ [] = 0
infracaoPorHora h (((hs,_),_,_,_):xs) | h == hs = 1 + infracaoPorHora h xs
                                      | otherwise = infracaoPorHora h xs

--c)

horaMaior :: Hora -> Hora -> Bool
horaMaior (h1,m1) (h2,m2) = h1 == h2 && m1 == m2 || h1 == h2 && m1 < m2 || h1 < h2
        
radarValid :: Radar -> Bool
radarValid [] = True
radarValid [x] = True
radarValid ((h1,_,_,_):y@(h2,_,_,_):xs) = horaMaior h1 h2 && radarValid (y:xs)

--Exercicio 7

type TabAbrev = [(Abreviatura,Palavra)]
type Abreviatura = String
type Palavra = String

--a)

existe :: Abreviatura -> TabAbrev -> Bool
existe _ [] = False
existe s ((a,_):xs) = s == a || existe s xs

--b)

substitui :: [String] -> TabAbrev -> [String]
substitui (x:xs) l = (tradeword x l) : substitui xs l 

tradeword :: String -> TabAbrev -> String
tradeword s [] = s
tradeword s ((ab,pal):xs) | s == pal = ab
                          | otherwise = tradeword s xs

--c)

estaOrdenada :: TabAbrev -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada ((ab,_):y@(abs,_):xs) = ab < abs && estaOrdenada (y:xs)

--Exercicio 8

--a)

f l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

{-
f "otrec"
g "o" "trec"
g "to" "rec"
g "rto" "ec"
g "erto" "c"
g "certo" []
f "certo"
-}

--b)

type Monomio = (Float,Int) -- (Coeficiente, Expoente)
type Polinomio = [Monomio]

--I)

coef :: Polinomio -> Int -> Float
coef [] _ = 0
coef ((c,e):xs) g | g == e = c
                  | otherwise = coef xs g

--II)

poliOk :: Polinomio -> Bool
poliOk [] = True
poliOk ((c,e):xs) = c /= 0 && not (elem e (map snd xs)) && poliOk xs

--Exercicio 9

--a)

size :: MSet a -> Int
size [] = 0
size ((_,i):xs) = i + size xs

--b)

union :: Eq a => MSet a -> MSet a -> MSet a
union (x:xs) ys = union xs (adicionar x ys)

adicionar :: Eq a => (a,Int) -> MSet a -> MSet a
adicionar (a,i) [] = [(a,i)]
adicionar (a,i) ((as,is):xs) | a == as = ((as,is+i):xs)
                             | otherwise = (as,is):(adicionar (a,i) xs)

--Exercicio 10

--a)

maisQUmaInfracao :: Radar -> Bool
maisQUmaInfracao xs = existeRepetidos (map (\(_, m, _, _) -> m) xs)


existeRepetidos :: Eq a => [a] -> Bool
existeRepetidos [] = False
existeRepetidos (x:xs) = elem x xs || existeRepetidos xs

--b)

infCar :: Matricula -> Radar -> [(Hora,VelCond)]
infCar _ [] = []
infCar s ((h,m,va,vc):xs) | s == m = (h,vc-va):infCar s xs
                          | otherwise = infCar s xs

--Exercicio 11

type TabTemp = [(Data,Temp,Temp)]  -- (data, temp. mı́nima, temp. máxima)
type Data = (Int,Int,Int)          -- (ano, mes, dia)
type Temp = Float 

--a)

médias :: TabTemp -> [(Data,Temp)]
médias [] = []
médias ((d,tmin,tmax):xs) = (d,(tmax+tmin)/2) : médias xs

--b)

decrescente :: TabTemp -> Bool
decrescente [] = True
decrescente [x] = True
decrescente ((d1,_,_):y@(d2,_,_):xs) = d1 > d2 && decrescente (y:xs)

--c)

conta :: [Data] -> TabTemp -> Int
conta _ [] = 0
conta ds ((d,_,_):xs) | elem d ds = 1 + conta ds xs
                      | otherwise = conta ds xs

--Exercicio 12

--a)

sameTime :: Radar -> Bool
sameTime [] = False
sameTime ((h,_,_,_):xs) = existeTime h xs || sameTime xs

existeTime :: Hora -> Radar -> Bool
existeTime _ [] = False
existeTime (h,m) (((h1,m1),_,_,_):xs) | h == h1 = True 
                                      | otherwise = existeTime (h,m) xs

--b)

maiorInf :: Radar -> (Hora,Matricula,VelAutor,VelCond)
maiorInf [x] = x
maiorInf (i@(_,_,va,vc):xs) | maxL > maxG = i
                            | otherwise = i2
        where
            maxL = (vc-va) 
            maxG = (vcl - val)
            i2@(_,_,vcl,val) = maiorInf xs

--c)

menortempoSemInfrações :: Radar -> Int
menortempoSemInfrações [(h,_,_,_),(h2,_,_,_)] = hora2min h2 - hora2min h
menortempoSemInfrações ((h,_,_,_):y@(h2,_,_,_):l) = min (hora2min h2 - hora2min h)  (menortempoSemInfrações (y:l))

--Exercicio 13

--a)

f' l = g' [] l
g' l [] = l
g' l (h:t) = g' (h:l) t

{-
f' "exif"
g' [] "exif"
g' "e" "xif"
g' "xe" "if"
g' "ixe" "f"
g' "fixe" []
f' "fixe
-}

--b)

--I)

addM :: Polinomio -> Monomio -> Polinomio
addM [] (b,e) = [(b,e)]
addM ((b,e):xs) (bs,es) | e == es = (((b+bs),e):xs)
                        | otherwise = (b,e):(addM xs (bs,es))

--II)

addP :: Polinomio -> Polinomio -> Polinomio
addP [] pl = pl
addP (x:xs) pl = addP xs (addM pl x)

--Exercicio 14

--a)

elem' :: Eq a => a -> MSet a -> Bool
elem' _ [] = False
elem' x ((c,i):xs) | c == x = True
                   | otherwise = elem' x xs

--b)

converte :: Eq a => [a] -> MSet a
converte [] = []
converte (x:xs) = insert' x (converte xs)

insert' :: Eq a => a -> MSet a -> MSet a
insert' a [] = [(a,1)]
insert' a ((c,i):xs) | a == c = (c,i+1):xs
                     | otherwise = ((c,i):(insert' a xs))