--Recurso 20_21

--Exercicio 1

type TabAbrev = [(Abreviatura,Palavra)]
type Abreviatura = String
type Palavra = String

ordena :: TabAbrev -> TabAbrev 
ordena [] = []
ordena (x:xs) = insertPal x (ordena xs)

insertPal :: (Abreviatura,Palavra) -> TabAbrev -> TabAbrev
insertPal x [] = [x]
insertPal x (l:ls) | snd x > snd l = l:insertPal x ls
                   | otherwise = x:l:ls

--Exercicio 2

type PlayList = [Musica]
type Musica = (Titulo,Interprete,Album,Duracao)
--type Titulo = String
type Interprete = String
type Album = String
type Duracao = Int       -- duração da música em segundos

choosePlayList :: PlayList -> [Album] -> PlayList
choosePlayList [] _ = []
choosePlayList (m@(_,_,a,_):ls) album | elem a album = m:choosePlayList ls album
                                      | otherwise = choosePlayList ls album

--Exercicio 3

type Imagem = Matriz Pixel 
type Matriz a = [[a]]
type Pixel = (Int,Int,Int) 

transforma :: Int -> Imagem -> Imagem
transforma x  = map (transPixel x) 

transPixel :: Int -> [Pixel] -> [Pixel]
transPixel x  = map (\(a,b,c) -> (a-x,b-x,c-x)) 

{-
--Exercicio 4

data Pokedex = Empty | Pokemons Pokemon Pokedex Pokedex
type Pokemon = (Nome, Stamina, Tipo)
data Tipo = Agua | Fogo | Terra | Ar
type Nome = String 
type Stamina = Int

p1 :: Pokedex
p1 = Pokemons ("Psyduck",90,Agua) 
   (Pokemons ("Charmander",55,Fogo) Empty Empty)  
  (Pokemons ("Blastoid",95,Agua) Empty Empty)

pokemons :: Int -> Pokedex -> [(Nome,Int)]
pokemons _ Empty = []
pokemons sta (Pokemons (n,s,_) e d) | sta <= s = pokemons sta e
                                    | sta > s = pokemons sta e ++ [(n,s)] ++ pokemons sta d
-}

--Exercicio 5

type Biblio = BTree Livro
data Livro = Prosa Titulo Autor Ano Lido
           | Poesia Titulo Autor Ano Lido
type Titulo = String
type Autor  = String
type Ano    = Int
data Lido   = Sim | Nao
  deriving Show

p1 :: BTree Livro
p1 = Node (Prosa "Psyduck" "Oak" 1900 Sim) 
   (Node (Prosa "AAA" "Oak" 1900 Sim) Empty Empty)  
  (Node (Poesia "ZZZ" "Oak" 1900 Nao) Empty Empty)

info :: Biblio -> Titulo -> Maybe Lido
info Empty _ = Nothing 
info (Node (Prosa t a ano l) e d) tit | tit == t = Just l
                                      | tit < t  = info e tit
                                      | tit > t  = info d tit
info (Node (Poesia t a ano l) e d) tit | tit == t = Just l
                                       | tit < t  = info e tit
                                       | tit > t  = info d tit

--Exercicio 6

func :: Int -> [Int] -> Int 
--func x l = foldr (*) 1 (filter (>x) l)
func _ [] = 1
func x (l:ls) | l > x = l * func x ls
              | otherwise = func x ls

--Exercicio 7

data BTree a = Empty | Node a (BTree a) (BTree a)


p2 :: BTree Float
p2 = Node 3.0 
   (Node 1.0 Empty Empty)  
  (Node 5.0 Empty Empty)

mostra :: BTree Float -> IO ()
mostra t = do
      putStrLn "Insira um valor:"
      c <- getLine 
      let x = read c :: Float
          l = searchFloat t x
      if null l then putStrLn "Não existem valores maiores que o valor inserido!!!"
                else 
                  putStrLn l

searchFloat :: BTree Float -> Float -> String
searchFloat Empty _ = []
searchFloat (Node a e d ) x | x == a = transTree d
                            | x > a  = searchFloat d x
                            | x < a  = searchFloat e x ++ show a ++ ", " ++ transTree d

transTree :: BTree Float -> String
transTree Empty = []
transTree (Node a Empty Empty) = show a
transTree (Node a e d) = transTree e ++ show a ++ ", " ++ transTree d