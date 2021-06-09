--Epoca Especial 17_18

--Exercicio 1

myExclamation :: [a] -> Int -> a
myExclamation [] n = error "posicao invalida"
myExclamation (x:xs) n | n == 0 = x
                       | otherwise = myExclamation xs (n-1)

--Exercicio 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (l:ls) = case l of
                Norte -> posicao (x,y+1) ls
                Sul   -> posicao (x,y-1) ls
                Oeste -> posicao (x-1,y) ls
                Este  -> posicao (x+1,y) ls

--Exercicio 3

any1 :: (a -> Bool) -> [a] -> Bool
any1 f [] = False
any1 f (l:ls) = (f l) || (any1 f ls)

--Exercicio 4

type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup m = quantZero m 0

quantZero :: (Eq a, Num a) => Mat a -> Int -> Bool 
quantZero [] _ = True 
quantZero (m:ms) n = quantlin m n && quantZero ms (n+1)

quantlin :: (Eq a, Num a) => [a] -> Int -> Bool 
quantlin [] n = True 
quantlin (x:xs) n | x == 0 && n > 0 = quantlin xs (n-1)
                  | n == 0 = True 
                  | otherwise = False

--Exercicio 5

movimenta :: IO (Int,Int)
movimenta = do 
        let posInicial = (0,0)
        putStrLn "Rules:"
        putStrLn "  To move Up Press 'N'"
        putStrLn "  To move Down Press 'S'"
        putStrLn "  To move Right Press 'E'"
        putStrLn "  To move Left Press 'O'" 
        putStrLn "  To End game Press 'Z'"
        putStrLn "Move Start!!!"
        putStrLn "Insert Mov"
        c <- getLine
        let mov = read c :: Char
        atualizaMove mov posInicial


atualizaMove :: Char -> (Int,Int) -> IO (Int,Int)
atualizaMove c (x,y) = do
                if c == 'N' then do
                            putStrLn "New move"
                            nc <- getLine 
                            let mov = read nc :: Char
                            atualizaMove mov (x,y+1)
                          else if c == 'S' then do
                                putStrLn "New move"
                                nc <- getLine 
                                let mov = read nc :: Char
                                atualizaMove mov (x,y-1)
                                else if c == 'O' then do
                                    putStrLn "New move"
                                    nc <- getLine 
                                    let mov = read nc :: Char
                                    atualizaMove mov (x-1,y)
                                    else if c == 'E' then do
                                        putStrLn "New move"
                                        nc <- getLine 
                                        let mov = read nc :: Char
                                        atualizaMove mov (x+1,y)
                                        else if c == 'Z' then do
                                            putStrLn "End Game!!!"
                                            putStrLn "Final Posicion: "
                                            return (x,y)
                                            else do
                                                putStrLn "Invaled Move, try again"
                                                nc <- getLine 
                                                let mov = read nc :: Char
                                                atualizaMove mov (x+1,y)

--Exercicio 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

--a)

vazia :: Imagem -> Bool
vazia (Quadrado x) = False 
vazia (Juntar []) = True 
vazia (Mover (_,_) i) = vazia i
vazia (Juntar (l:ls)) = vazia l && vazia (Juntar ls)

--b)

maior :: Imagem -> Maybe Int
maior (Juntar []) = Nothing 
maior (Quadrado x) = Just x
maior (Mover (_,_) i) = maior i 
maior (Juntar l@((Quadrado x):ls)) = Just (maiorAcc x (Juntar l))
maior (Juntar (l:ls)) | maior l > maior (Juntar ls) = maior l
                      | otherwise = maior (Juntar ls)
    
maiorAcc :: Int -> Imagem -> Int 
maiorAcc n (Juntar []) = n
maiorAcc n (Quadrado x) | x > n = x
                        | otherwise = n
maiorAcc n (Mover (_,_) i) = maiorAcc n i
maiorAcc n (Juntar (l:ls)) = maiorAcc (maiorAcc n l) (Juntar ls)

--c)

instance Eq Imagem where
    img1 == img2 = null $ (quadPos img1 (0,0)) \\ (quadPos img2 (0,0)) 

quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) img) (x,y) = quadPos img (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x (pos)) imgs