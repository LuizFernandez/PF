--Mini - Teste 4
{-
--Exercicio 1

--a)

data BTree a = Vazia | Nodo a (BTree a) (BTree a)

filtra :: (a->Bool) -> (BTree a) -> [a]
filtra f Vazia = []
filtra f (Nodo x e d) | f x = x : (filtra f e) ++ (filtra f d)
                      | otherwise = (filtra f e) ++ (filtra f d)

--b)

data Contacto = Casa Integer
               |Trab Integer
               |Tlm Integer
               |Email String

type Nome = String
type Agenda = [(Nome, [Contacto])]

--I)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n,[Email e])]
acrescEmail n e ((ns,c):as) | n == ns = ((n,(Email e):c):as)
                            | otherwise = (n,c):(acrescEmail n e as)

--II)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((ns,c):as) | n == ns = Just (searchMail c)
                        | otherwise = verEmails n as

searchMail :: [Contacto] -> [String]
searchMail [] = []
searchMail ((Email e) : cs) = (e:searchMail cs)
searchMail (_:cs) = (searchMail cs)
-}
{-
--Exercicio 2

data LTree a = Leaf a | Fork (LTree a) (LTree a)

--a)

instance Show a => Show (LTree a) where
    show (Leaf x)   = show x
    show (Fork a b) = "(" ++ show a ++ "/\" ++ show b ++ ")"

--b)   

mktree :: Int -> a -> LTree a
mktree x y = build (replicate x y)

-}
{-
--Exercicio 3

--type Dakar = [Piloto]
data Piloto = Carro Numero Nome Categoria
            | Mota Numero Nome Categoria
            | Camiao Numero Nome

type Numero = Int
--type Nome = String

data Categoria = Competicao | Maratona

--a)

searchSameNume :: Dakar -> Bool
searchSameNume d = existeRepetidos $ map organizeNum d

existeRepetidos :: [Numero] -> Bool
existeRepetidos [] = False
existeRepetidos (x:xs) = elem x xs || existeRepetidos xs

organizeNum :: Piloto -> Numero
organizeNum (Carro n _ _) = n 
organizeNum (Mota n _ _) = n 
organizeNum (Camiao n _) = n 

--b)

inserePil :: Piloto -> Dakar -> Dakar
inserePil p [] = [p]
inserePil p (d:ds) | organizeNum p > organizeNum d = d : inserePil p ds
                   | otherwise = p:d:ds

--c)

--data BTree a = Vazia | Nodo a (BTree a) (BTree a)
type Dakar = BTree Piloto

menor :: Dakar -> Piloto
menor (Nodo a Vazia _) = a
menor (Nodo a e _) = menor e 
-}
{-
--Exercicio 4

--a)

minimo :: (Ord a) => BTree a -> a
minimo (Nodo a Vazia _) = a
minimo (Nodo a e _) = minimo e

--b)

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Nodo a Vazia d) = d
semMinimo (Nodo a e d) = Nodo a (semMinimo e) d

--c)

minSmin :: (Ord a) => BTree a -> (a,BTree a)
minSmin (Nodo a Vazia d) = (a,d)
minSmin (Nodo a e d) = (min,Nodo a newt d)
    where
        (min, newt) = minSmin e
-}
{-
--Exercicio 5

--a)

data ArvBin a = Vazia | Nodo a (ArvBin a) (ArvBin a)

minAB :: ArvBin Int -> Maybe Int
minAB Vazia = Nothing
minAB (Nodo a Vazia _) = Just a
minAB (Nodo a e _) = minAB e

--b)

type BD = [Video]
data Video = Filme String Int -- tı́tulo, ano
            | Serie String Int Int -- tı́tulo, temporada, episódio
            | Show String Int -- tı́tulo, ano
            | Outro String

--I)

espectaculos :: BD -> [(String,Int)]
espectaculos [] = []
espectaculos ((Show s a):bds) = (s,a):(espectaculos bds)
espectaculos (_:bds) = espectaculos bds

--II)

filmesAno :: Int -> BD -> [String]
filmesAno _ [] = []
filmesAno ano ((Filme s a):bds) | ano == a = s:(filmesAno bds)
                                | otherwise = filmesAno bds
filmesAno ano (_:bds) = filmesAno ano bds
-}
{-
--Exercicio 6

data LTree a = Leaf a | Fork (LTree a) (LTree a)

--a)

instance Eq a => Eq (LTree a) where
    Leaf a   == Leaf b   = a == b
    Fork a b == Fork c d = a == c && b == d
    _        == _        = False

--b)

mapLT :: (a -> b) -> LTree a -> LTree b
mapLT f (Leaf a) = Leaf (f a)
mapLT f (Fork a b) = Fork (mapLT f a) (mapLT f b) 
-}
{-
--Exercicio 7

type Biblio = [Livro]
data Livro = Romance Titulo Autor Ano Lido
           | Ficcao Titulo Autor Ano Lido
type Titulo = String
type Autor = String
type Ano = Int
data Lido = Sim | Nao

--a)

instance Eq Livro where
    Romance t1 at1 a1 _ == Romance t2 at2 a2 _ = (t1 == t2) && (at1 == at2) && (a1 == a2)
    Ficcao t1 at1 a1 _  == Ficcao t2 at2 a2 _  = (t1 == t2) && (at1 == at2) && (a1 == a2)
    _                   == _                   = False

checkRep :: Biblio -> Bool
checkRep xs = existeRepetidos xs

existeRepetidos :: Biblio -> Bool
existeRepetidos [] = False
existeRepetidos (x:xs) = elem x xs || existeRepetidos xs

--b)

instance Eq Lido where
    Nao == Nao = True
    Sim == Sim = True
    _   == _   = False

lido :: Biblio -> Titulo -> Biblio
lido [] _ = []
lido (y@(Romance t at a l):bs) titulo | titulo == t && l == Nao = (Romance t at a Sim):bs
                                      | titulo == t = y:bs
                                      | otherwise = y:(lido bs titulo)
lido (y@(Ficcao t at a l):bs) titulo  | titulo == t && l == Nao = (Ficcao t at a Sim):bs
                                      | titulo == t = y:bs
                                      | otherwise = y:(lido bs titulo)

--c)

data BTree a = Vazia | Nodo a (BTree a) (BTree a)
type Biblio2 = BTree Livro

livroAutor :: Biblio2 -> Autor -> [Livro]
livroAutor Vazia _ = []
livroAutor (Nodo y@(Romance t at _ _) e d) autor | at == autor = [y] ++ (livroAutor e autor) ++ (livroAutor d autor)
                                                 | otherwise = (livroAutor e autor) ++ (livroAutor d autor)
livroAutor (Nodo y@(Ficcao t at _ _) e d) autor  | at == autor = [y] ++ (livroAutor e autor) ++ (livroAutor d autor)
                                                 | otherwise = (livroAutor e autor) ++ (livroAutor d autor)
-}
{-
--Exercicio 8

--a)

data BTree a = Vazia | Nodo a (BTree a) (BTree a)

mapBT :: (a->b) -> (BTree a) -> (BTree b) 
mapBT f Vazia = Vazia
mapBT f (Nodo a e d) = Nodo (f a) (mapBT f e) (mapBT f d)

--b)

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String

type Nome = String
type Agenda = [(Nome, [Contacto])]

--I)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa t):cs) = t:(consTelefs cs)
consTelefs ((Trab t):cs) = t:(consTelefs cs)
consTelefs ((Tlm t):cs)  = t:(consTelefs cs)
consTelefs (_:cs)        = consTelefs cs

--II)

casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((ns,c):cs) | n == ns = takeNum c
                   | otherwise = casa n cs

takeNum :: Contacto -> Maybe Integer
takeNum [] = Nothing
takeNum ((Casa t):cs) = Just t
takeNum (_:cs) = takeNum cs
-}
{-
--Exercicio 9

data LTree a = Leaf a | Fork (LTree a) (LTree a)

--a)

instance Eq a => Eq (LTree a) where
    Leaf a   == Leaf b   = a == b
    Fork a b == Fork c d = a == c && b == d
    _        == _        = False

--b)

build :: [a] -> LTree a
build [x] = Leaf x
build (x:y:xs) = Fork (Fork (Leaf x) (Leaf y)) (build xs)
-}
{-  
--Exercicio 10

--a)

data BTree a = Empty | Node a (BTree a) (BTree a)

maxBT :: BTree Float -> Maybe Float
maxBT Empty = Nothing
maxBT (Node a e Empty) = Just a
maxBT (Node a _ d) = maxBT d

--b)

type BD = [Video]
data Video = Filme String Int -- tı́tulo, ano
           | Serie String Int Int -- tı́tulo, temporada, episódio
           | Show  String Int -- tı́tulo, ano
           | Outro String

--I)

outros :: BD -> BD
outros [] = []
outros ((Outro s):bd) = (Outro s):(outros bd)
outros (_:bd) = outros bd

--II)

totalEp :: String -> BD -> Int
totalEp s [] = 0
totalEp s ((Serie titulo temp ep):cs) | s == titulo = ep + (totalEp s cs)
                                      | otherwise = totalEp s cs
totalEp s (_:cs) = totalEp s cs
-}
{-
--Exercicio 11

data LTree a = Leaf a | Fork (LTree a) (LTree a)

--a)

instance Show a => Show (LTree a) where
    show lt = unlines $ map (\(n,d) -> (replicate d '.' ++ show n)) (travessia lt)

travessia :: LTree a -> [(a,Int)]
travessia (Leaf a) = [(a,0)]
travessia (Fork a b) = map(\(n,e) -> (n, e+1)) ((travessia a) ++ (travessia b))

--b)

cresce :: LTree a -> LTree a
cresce (Leaf x) = (Fork (Leaf x) (Leaf x))
cresce (Fork a b) = (Fork (cresce a) (cresce b))
-}
{-
--Exercicio 12

type Dakar = [Piloto]
data Piloto = Carro Numero Nome Categoria
            | Mota Numero Nome Categoria
            | Camiao Numero Nome

type Numero = Int
type Nome = String
data Categoria = Competicao | Maratona

--a)

inserePil :: Piloto -> Dakar -> Dakar
inserePil p [] = [p]
inserePil p (x:xs) | nomePil p < nomePil x = p:x:xs
                   | otherwise = x : (inserePil p xs)

nomePil :: Piloto -> Nome
nomePil (Carro _ n _) = n
nomePil (Mota _ n _) = n
nomePil (Camiao _ n) = n

--b)

checkOrd :: Dakar -> Bool
checkOrd [] = True
checkOrd [x] = True
checkOrd (x:y:xs) = nomePil x < nomePil y && (checkOrd (y:xs))

--c)

data BTree a = Vazia | Nodo a (BTree a) (BTree a)
type Dakarb = BTree Piloto

maior :: Dakarb -> Piloto
maior (Nodo p e Vazia ) = p 
maior (Nodo p _ d) = maior d
-}
{-
--Exercicio 13

data BTree a = Vazia | Nodo a (BTree a) (BTree a)

--a)

maximo :: (Ord a) => BTree a -> a
maximo (Nodo a _ Vazia) = a
maximo (Nodo a _ d) = maximo d

--b)

semMaximo :: (Ord a) => BTree a -> BTree a
semMaximo (Nodo a e Vazia) = e
semMaximo (Nodo a e d) = (Nodo a e (semMaximo d))

--c)

maxSmax :: (Ord a) => BTree a -> (a,BTree a)
maxSmax (Nodo a e Vazia) = (a,e)
maxSmax (Nodo a e d) = (max, Nodo a e maxTree)
    where
        (max,maxTree) = maxSmax d
-}
--Exercicio 14

type Biblio = [Livro]
data Livro = Romance Titulo Autor Ano Lido
           | Ficcao Titulo Autor Ano Lido

type Titulo = String
type Autor = String
type Ano = Int
data Lido = Sim | Nao

--a)

instance Eq Lido where
    Sim == Sim = True
    Nao == Nao = True
    _   == _   = False

livroLidos :: Biblio -> Int
livroLidos [] = 0
livroLidos ((Romance _ _ _ l):cs) | l == Sim = 1 + (livroLidos cs)
                                  | otherwise = (livroLidos cs)
livroLidos ((Ficcao _ _ _ l):cs)  | l == Sim = 1 + (livroLidos cs)
                                  | otherwise = (livroLidos cs)

--b)

compra :: Titulo -> Autor -> Ano -> Biblio -> Biblio
compra t at ano [] = [(Romance t at ano Nao)]
compra t at ano (c:cs) | autor c < t = c:(compra t at ano cs)
                       | otherwise = ((Romance t at ano Nao):c:cs)

autor :: Livro -> Autor
autor (Romance _ autor _ _) = autor
autor (Ficcao _ autor _ _)  = autor

--c)

data BTree a = Vazia | Nodo a (BTree a) (BTree a)
type Bibliob = BTree Livro

naoLidos :: Bibliob -> [Livro]
naoLidos Vazia = []
naoLidos (Nodo a e d) | jaLido a == Sim = (naoLidos e) ++ (naoLidos d)
                      | otherwise = (naoLidos e) ++ [a] ++ (naoLidos d)

jaLido :: Livro -> Lido
jaLido (Romance _ _ _ l) = l
jaLido (Ficcao _ _ _ l) = l