--Teste 19/20

--Exercicio 1

--a)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) l | elem1 x l = x:intersect xs l 
                   | otherwise = intersect xs l

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False 
elem1 n (l:ls) = n == l || elem1 n ls

--b)

tails :: [a] -> [[a]]
tails [] = [[]]
tails l  = [l] ++ tails (tail1 l)

tail1 :: [a] -> [a]
tail1 [] = []
tail1 (l:ls) = ls

--Exercicio 2

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a)    

elems :: ConjInt -> [Int]
elems [] = []
elems (l:ls) = dCompress l ++ elems ls

dCompress :: (Int,Int) -> [Int]
dCompress (x,y) | x == y = [x]
                | otherwise = x:dCompress (x+1,y)

--b)

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (l:ls) = map zipPares (geraMatriz [l] ls)

geraMatriz :: [Int] -> [Int] -> [[Int]]
geraMatriz l [] = [l]
geraMatriz l (x:y) | last l == x-1 = geraMatriz (l++[x]) y
                   | otherwise = l : geraMatriz [x] y

zipPares :: [Int] -> (Int,Int)
zipPares l = (head l,last l)

--Exercicio 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
        deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

--a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n,[Email s])]
acrescEmail n s ((ns,c):ls) | n == ns = ((ns,(Email s):c):ls)
                            | otherwise = (ns,c):acrescEmail n s ls

--b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing 
verEmails n ((ns,c):ls) | n == ns = Just (map mail c)
                        | otherwise = verEmails n ls

mail :: Contacto -> String
mail (Email s) = s 
mail _ = []

--c)

consulta :: [Contacto] -> ([Integer],[String])
consulta  = consultaAcc ([],[]) 
        where
            consultaAcc (t,e) [] = (t,e)
            consultaAcc (t,e) ((Email s):ls) = consultaAcc (t,e++[s]) ls
            consultaAcc (t,e) ((Trab tel):ls) = consultaAcc (t++[tel],e) ls 
            consultaAcc (t,e) ((Tlm tel):ls) = consultaAcc (t++[tel],e) ls
            consultaAcc (t,e) ((Casa tel):ls) = consultaAcc (t++[tel],e) ls  

--d)

consultaIO :: Agenda -> IO ()
consultaIO a = do
        putStr "Nome a procurar:"
        nome <- getLine 
        let c = getContacto nome a
        if noContactos c then do
                            putStr "Nome inválido"
                            consultaIO a
                        else do
                            putStrLn "Conatactos: " 
                            showContactos c

getContacto :: String -> Agenda -> [Contacto]
getContacto n [] = []
getContacto n ((ns,c):ls) | n == ns = c
                          | otherwise = getContacto n ls

showContactos :: [Contacto] -> IO ()
showContactos [] = putStrLn "Fim da Lista!"
showContactos (c:cs) = do
            putStrLn (show c)
            showContactos cs

noContactos :: [Contacto] -> Bool 
noContactos [] = True 
noContactos l  = False

consultaIOVer2 :: Agenda -> IO()
consultaIOVer2 a = do
            putStrLn "Nome a procurar:"
            c <- getLine
            let nome = read c :: String
                contac = searchContactos nome a
            if null contac then do 
                        putStrLn "Nome invalido ou Nome não apresenta contactos"
                        consultaIOVer2 a
                            else do
                                let r = readContactos contac
                                putStr r

searchContactos :: String -> Agenda -> [Contacto]
searchContactos _ [] = []
searchContactos nome ((n,c):ls) | nome == n = c
                                | otherwise = searchContactos nome ls

readContactos :: [Contacto] -> String
readContactos [] = []
readContactos ((Email e):ls) = "Email: " ++ e ++ "\n" ++ readContactos ls
readContactos ((Trab t) :ls) = "Trab: " ++ "t" ++ "\n" ++ readContactos ls
readContactos ((Tlm t) :ls)  = "Tlm: " ++ "t" ++ "\n" ++ readContactos ls
readContactos ((Casa t):ls)  = "Casa: " ++ "t" ++ "\n" ++ readContactos ls

--Exercicio 4

data RTree a = R a [RTree a] deriving (Show, Eq)

--a)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R n l) = map (\x -> n : x) filhos
    where
        filhos = concat (map paths l)

--b)

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths l = R node (map unpaths restosAgrupados)
    where 
        node = head (head l)
        restos = map tail l
        nodosFilhos = tiraRepetidos (map head restos)
        restosAgrupados = ragrAux restos nodosFilhos

ragrAux :: Eq a => [[a]] -> [a] -> [[[a]]]
ragrAux restos [] = []
ragrAux restos (x:xs) = (filter (\l -> head l == x) restos) : ragrAux restos xs

tiraRepetidos :: Eq a => [a] -> [a]
tiraRepetidos [] = []
tiraRepetidos (x:xs) | elem x xs = tiraRepetidos xs
                     | otherwise = x : tiraRepetidos xs