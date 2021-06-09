--Mini - Teste 3

--Exercicio 1

type RegAlcool = [(Nome,Sexo,Idade,NA)]
type Nome = String
type Sexo = Char -- ’M’: Masculino ’F’: Feminino
type Idade = Int
type NA = Float -- Nivel de Alcool

--a)

menores21 :: RegAlcool -> RegAlcool
menores21 l = filter (\(_,_,i,_) -> i <= 21) l

--b)

multa :: RegAlcool -> [(Nome,Idade,Float)]
multa l = map(\(n,_,i,na) -> (n,i,if na < 0.5 then 0 else na * 100)) l

--c)

mediaIdade :: RegAlcool -> Float
mediaIdade l = fromIntegral(sum (map (\(_,_,i,_) -> i) l)) / fromIntegral (length l)

--Exercicio 2

--a)

func :: Eq a => a -> [a] -> Bool
func x l = not (null (filter (x==) l))

func' :: Eq a => a -> [a] -> Bool
func' _ [] = False
func' x (l:ls) = l == x || func' x ls

--b)

type Matriz a = [[a]]

zero :: (Eq a, Num a) => Matriz a -> Bool
zero m = and $ map (all (==0)) m

--Exercicio 3

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)

conta :: Int -> Polinomio -> Int
conta x l = length $ filter (==x) (map snd l)

--b)

selgrau :: Int -> Polinomio -> Polinomio
selgrau x l = filter ((>x).snd) l

--c)

deriv :: Polinomio -> Polinomio
deriv p = map (\(b,e) -> (b*(fromIntegral e),e-1)) p

--Exercicio 4

type Radar = [(Hora,Matricula,VelAutor,VelCond)]
type Hora = (Int,Int)
type Matricula = String -- matricula do carro em infraccao
type VelAutor = Int -- velocidade autorizada
type VelCond = Float -- velocidade do condutor

--a)

radarValid :: Radar -> Bool
radarValid l = null (filter (\(_,_,va,vc) -> vc < (fromIntegral va)) l)

--b)

resumoDiario :: Radar -> [(Matricula,Float)]
resumoDiario r = map(\(_,m,va,vc) -> (m,vc - (fromIntegral va))) r
               
--c)

excessoTotal :: [(Matricula,Float)] -> Float
excessoTotal l = sum $ map snd l

--Exercicio 5

--a)

func5 :: [[a]] -> [Int]
func5 l = map length (filter null l)

func5Alt :: Eq a => [[a]] -> [Int]
func5Alt [] = []
func5Alt (m:ms) | m == [] = 0 : func5Alt ms
                | otherwise = func5Alt ms

--b)

type MSet a = [(a,Int)]

elem :: Eq a => a -> MSet a -> Bool
elem x l = any (==x) (map fst l)

--Exercicio 6

--a)

--I)

criaPares :: a -> [b] -> [(a,b)]
criaPares _ [] = []
criaPares x (l:ls) = (x,l):criaPares x ls

--II)

criaPares' :: a -> [b] -> [(a,b)]
criaPares' a bs = map (acrescenta a) bs
    where
        acrescenta :: a -> b -> (a,b) 
        acrescenta a x = (a,x)

--b)

concat2 :: [[a]] -> [a]
concat2 m = foldl (++) [] m

--Exercicio 7

--a)

calcula :: Float -> Polinomio -> Float
calcula x l = sum $ map (\(b,e) -> b * x^e) l

--b)

simp :: Polinomio -> Polinomio
simp l = filter (\(b,e) -> b /= 0) l

--c)

mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(bs,es) -> (bs*c,e+es)) p

--Exercicio 8

--a)

onlyWomem :: RegAlcool -> RegAlcool
onlyWomem l = filter (\(_,s,_,_) -> s == 'F') l

--b)

validConducao :: RegAlcool -> [(Nome,String)]
validConducao l = map (\(n,_,_,na) -> (n,if na < 0.5 then "legal" else "ilegal")) l

--c)

valorMulta :: RegAlcool -> Float 
valorMulta l = sum $ map (\(_,_,_,na) -> if na < 0.5 then 0 else na *100) l

--Exercicio 9

--a)

func9 :: Eq a => [a] -> [a] -> Bool
func9 l m = and (zipWith (==) l m) 

func9alt :: Eq a => [a] -> [a] -> Bool
func9alt [] [] = True
func9alt [] _ = False
func9alt _ [] = False
func9alt (l:ls) (m:ms) = l == m && func9alt ls ms

--b)

quadrada :: Matriz a -> Bool
quadrada m = all (== length m) $ map length m

--Exercicio 10

--a)

tolerancia :: Radar -> Radar
tolerancia l = filter (\(_, _, va, vc) -> vc > (1.1*(fromIntegral va))) l

--b)

listaInfracao :: Matricula -> Radar -> Radar
listaInfracao m l = filter (\(_,mr,_,_) -> m == mr ) l

--c)

excessoTotalTole :: Radar -> Float
excessoTotalTole r = sum $ map(\(_,_,va,vc) -> (vc - (fromIntegral va))) (tolerancia r)

--Exercicio 11

type Jornada = [Jogo]
type Jogo = ((Equipa,Golos),(Equipa,Golos)) -- (eq.casa, eq. visitante)
type Equipa = String
type Golos = Int

--a)

totalGolos :: Jornada -> Int
totalGolos j = sum $ map(\((_,g1),(_,g2)) -> g1 + g2) j

--b)

numGolos :: Int -> Jornada -> [Jogo]
numGolos g j = filter (\((_,g1),(_,g2)) -> g1+g2 > g) j

--c)

venceCasa :: Jornada -> [Equipa]
venceCasa j = map casa (filter vc j)

vc :: Jogo -> Bool
vc ((_,g1),(_,g2)) = g1 > g2  

casa :: Jogo -> Equipa
casa ((e,_),(_,_)) = e 

--Exercicio 12

--a)

--I)

criaLinhasRec :: [a] -> [b] -> [[(a,b)]]
criaLinhasRec [] _ = []
criaLinhasRec _ [] = []
criaLinhasRec (x:xs) l = zip (repeat x) l : criaLinhasRec xs l

--II)

crialinhas :: [a] -> [b] -> [[(a,b)]]
crialinhas l1 l2 = map (f l2) l1
    where 
        f l x = zip (repeat x) l

--b)

concat'' :: [[a]] -> [a]
concat'' m = foldl (++) [] m

--Exercicio 13

--a)

func13 :: Ord a => a -> [a] -> Int
func13 x l = length (filter (>= x) l)

func13Rec :: Ord a => a -> [a] -> Int
func13Rec x [] = 0
func13Rec x (l:ls) | l >= x = 1+ func13Rec x ls 
                   | otherwise = func13Rec x ls

--b)

size :: MSet a -> Int
size l = sum $ map snd l

--Exercicio 14

--a)

pontos :: Jornada -> [(Equipa,Int)]
pontos j = concat $ map pontosJogo j
    where        
        pontosJogo ((e1,g1),(e2,g2)) | g1 == g2 = [(e1,1),(e2,1)]
                                     | g1 > g2 = [(e1,3),(e2,0)]
                                     | otherwise = [(e1,0),(e2,3)]

--b)

empates :: Jornada -> [Jogo]
empates j = filter (\((e1,g1),(e2,g2)) ->  g1 == g2) j

--c)

golosMarcados :: Jornada -> Int
golosMarcados j = sum (map soma j)

soma :: Jogo -> Int
soma ((_,g1),(_,g2)) = g1 + g2