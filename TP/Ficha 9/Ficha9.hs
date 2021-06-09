
module Ficha9 where
    

import Data.Char
import Data.List
import Data.Maybe
import System.Random

f :: Int -> Int
f x = x*2

main :: IO ()
main = do
    i <- main1
    main2 i

main1 :: IO Int
main1 = do
    str <- getLine
    let x2 = f (read str)
    return x2

main2 :: Int -> IO ()
main2 x = print x

main3 :: IO ()
main3 = do 
    str <- readFile "Palavras.txt" -- lê uma lista
    let xs = lines str --adiciona uma \n a uma lista 
    let ys = sort xs -- ordena uma lists 
    let str' = unlines ys -- retira \n de uma lista
    writeFile "Palavras2.txt" str' -- criar um novo documento txt


--Ficha 9

--Exercicio 1

--a)

bingo :: IO ()
bingo = bingo' [1..90]

bingo' :: [Int] ->IO ()
bingo' [] = return ()
bingo' nums = do 
    print nums
    getLine -- mostrar no ecrã alguma coisa (Int, Lista, etc)
    i <- randomRIO  (0,length nums-1) -- Gerar numeros aleatorios entre dois valores
    let num = nums !! i
    printInt num
    bingo' (delete num nums)

printInt :: Int -> IO ()
printInt i = print i

--b)

mastermind :: IO ()
mastermind = do 
    i1 <- randomRIO (0,9)
    i2 <- randomRIO (0,9)
    i3 <- randomRIO (0,9)
    i4 <- randomRIO (0,9)
    let is = [i1,i2,i3,i4]
    mastermind' is

mastermind' :: [Int] -> IO ()
mastermind' is = do
    putStrLn "quero 4 digitos"
    u1 <- getChar
    u2 <- getChar
    u3 <- getChar 
    u4 <- getChar
    let us = [read [u1],read [u2], read [u3],read [u4]]
    print us
    let c = correto is us 0
    let i = incorreto is us 0
    if c == 4
        then putStrLn "ganhou!"
        else do
            print (c,i)
            mastermind' is



correto :: [Int] -> [Int] -> Int -> Int
correto [] l n = n
correto l ls n | head l == head ls = correto (tail l) (tail ls) (n+1)
               | otherwise = correto (tail l) (tail ls) (n+0)

incorreto :: [Int] -> [Int] -> Int -> Int
incorreto [] l n = n
incorreto l ls n | elem (head ls) l && head l /= head ls = incorreto (tail l) (tail ls) (n+1)
                 | otherwise = incorreto (tail l) (tail ls) (n+0)

--Exercicio 2

data Aposta = Ap [Int] (Int,Int)

--a)

valida :: Aposta -> Bool
valida (Ap (a:b:c:d:e:[]) (t,u)) = and [elem x [1..50] | x <- [a,b,c,d,e]] && and (map (\x -> elem x [1..9]) [t,u])
valida _ = False

--b)

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap a1@(a:b:c:d:e:[]) (x,y)) (Ap a2@(f:g:h:i:j:[]) (m,n)) = (nc,ec)
             where
                nc = length [x | x <- a1, elem x a2]
                ec = length [z | z <- [x,y], elem z [m,n]]

--c)

--I)

instance Eq Aposta where
    (==) a b = comuns a b == (5,2)

--II)

premio :: Aposta -> Aposta -> Maybe Int
premio a b = case comuns a b of
                   (5,n) -> Just (3-n)
                   (4,n) -> Just (6-n)
                   (3,2) -> Just 7
                   (2,2) -> Just 8
                   (3,n) -> Just (10-n)
                   (1,2) -> Just 11
                   (2,n) -> Just (13-n)
                   otherwise -> Nothing

--d)

--I)

leAposta :: IO Aposta
leAposta = do 
    print "Introduza os numeros (separados por um espaco):"
    nums <- getLine
    print "Introduza as estrelas (separadas por um espaco):"
    stars <- getLine
    let bet = (Ap (map (\x -> (read x) :: Int) (unspace nums)) (let (a:b:r) = (unspace stars) in ((read a) :: Int ,(read b):: Int)))
    if valida bet then return bet else do 
        print "Aposta invalida, tente novamente!"
        leAposta

unspace :: String -> [String]
unspace l = map (\x -> x:[]) l 

--II)

joga :: Aposta -> IO ()
joga ch = do
    ap <- leAposta
    print ((++) ("Premio: ") $ show $ fromMaybe 0 (premio ch ap))

--e)

geraChave :: IO Aposta
geraChave = do 
    nums <- generate 'N' []
    [st1,st2] <- generate 'S' []
    return (Ap nums (st1,st2))

generate :: Char -> [Int] -> IO [Int]
generate c l = do
    n <- randomRIO (1,(if c=='N' then 50 else 9))
    if length l == 5 && c=='N' then return l
                               else if length l == 2 && c=='S' then return l
                                                               else if elem n l then generate c l 
                                                                                else generate c (n:l)

--f)

main :: IO ()
main = do 
    ch <- geraChave 
    ciclo ch

ciclo :: Aposta -> IO ()
ciclo ap = do
    mOp <- menu
    case mOp of
        "1" -> do
            joga ap
            ; ciclo ap
        "2" -> do
            putStrLn "Nova chave gerada"; main
        "0" -> return ()

menu :: IO String
menu = do 
    { putStrLn menutxt
    ; putStr "Opcao: "
    ; c <- getLine
    ; return c
    }
    where 
        menutxt = unlines ["",
                           "Apostar ........... 1",
                           "Gerar nova chave .. 2",
                           "",
                           "Sair .............. 0"]


