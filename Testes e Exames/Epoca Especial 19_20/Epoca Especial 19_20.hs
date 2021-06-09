--Epoca Especial 19/20

--Exercicio 1

--a)

subst :: Eq a => (a,a) -> [a] -> [a]
subst _ [] = []
subst (x,y) (l:ls) | l == x = y:subst (x,y) ls
                   | otherwise = l:subst (x,y) ls

--b)

posicoes :: [a] -> [Int] -> [a]
posicoes l [] = []
posicoes l (m:ms) = lookPos l m:posicoes l ms
    where
        lookPos (l:ls) n | n == 1 = l
                         | otherwise = lookPos ls (n-1)

--Exercicio 2

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)

--a)

folhas :: Tree a b -> [b]
folhas (Leaf b) = [b]
folhas (Node a e d) = folhas e ++ folhas d

--b)

somas :: Tree Float Int -> (Float,Int)
somas = somasAcc (0,0) 
     where
         somasAcc (a,b) (Leaf x) = (a,b+x)
         somasAcc (a,b) (Node x e d) = somasAcc (somasAcc (a+x,b) d) e 

--Exercicio 3

type Mat a = [[a]]

rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m = rotateLeft restos ++ [map head m]
    where
        restos = map tail m

--Exercicio 4

type Filme = (Titulo,Realizador,[Actor],Genero,Ano)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int
data Genero = Comedia | Drama | Ficcao | Accao | Animacao | Documentario
        deriving (Eq, Show)
type Filmes = [Filme]

--a)

doRealizador :: Filmes -> Realizador -> [Titulo]
doRealizador [] _ = []
doRealizador ((t,rs,_,_,_):fs) r | r == rs = t : doRealizador fs r
                                 | otherwise = doRealizador fs r

--b)

doActor :: Filmes -> Actor -> [Titulo]
doActor [] _ = []
doActor ((t,_,a,_,_):fs) act | elem act a = t : doActor fs act
                             | otherwise = doActor fs act

--c)

consulta :: Filmes -> Genero -> Realizador -> [(Ano, Titulo)]
consulta bd gen rea = map aux (filter (teste gen rea) bd)
    where 
        teste :: Genero -> Realizador -> Filme -> Bool
        teste g r (_,x,_,y,_) = g==y && r==x
        aux :: Filme -> (Ano,Titulo)
        aux (t,_,_,_,a) = (a,t)

--Exercicio 5

data Avaliacao = NaoVi
               | Pontos Int -- pontuacao entre 1 e 5
               deriving Show
type FilmesAval = [(Filme,[Avaliacao])]

exemplo1 :: FilmesAval
exemplo1 = [(("aaa","r",[],Comedia,1),[Pontos 2, NaoVi,Pontos 4]),(("bbb","r",[],Accao,2),[NaoVi,NaoVi]),(("ccc","r",[],Comedia,3),[Pontos 3, Pontos 4, Pontos 5])]

--a)


avalia :: FilmesAval -> IO FilmesAval
avalia fa = do
        putStrLn "Selecione um titulo de um Filme que já tenha visto:"
        titulo <- getLine
        if filmeValido titulo fa then do
                                     putStrLn "Caso tenha visto insira uma Pontuação, de 0 a 5, sendo 0 caso nao tenha visto:"
                                     avaliacao <- getLine
                                     let p = read avaliacao :: Int
                                     if p == 0 then do 
                                            let rN = inserirPontuação titulo NaoVi fa
                                            return rN
                                               else do
                                            let rP = inserirPontuação titulo (Pontos p) fa
                                            return rP       
                                 else do
                                  putStrLn "Filme inválido, tente outro."
                                  avalia fa

filmeValido :: Titulo -> FilmesAval -> Bool 
filmeValido _ [] = False 
filmeValido t (((titulo,_,_,_,_),_):ls) = t == titulo || filmeValido t ls

inserirPontuação :: Titulo -> Avaliacao -> FilmesAval -> FilmesAval
inserirPontuação t p ((fs@(titulo,_,_,_,_),l):ls) | t == titulo = (fs,p:l):ls
                                                  | otherwise = (fs,l):inserirPontuação t p ls

--b)

listaPorGeneros :: FilmesAval -> [(Genero,[(Titulo,Avaliacao)])]
listaPorGeneros l =  removeFilmeEmpty (com : ac : dra : fic : ani : doc)
        where
            com = listaPorGenerosAcc Comedia l []
            ac  = listaPorGenerosAcc Accao l []
            dra = listaPorGenerosAcc Drama l []
            fic = listaPorGenerosAcc Ficcao l []
            ani = listaPorGenerosAcc Animacao l []
            doc = [listaPorGenerosAcc Documentario l []]
            listaPorGenerosAcc c f l = (c,searchGen c (titAvaliacao f))
            
removeFilmeEmpty :: [(Genero,[(Titulo,Avaliacao)])] -> [(Genero,[(Titulo,Avaliacao)])]
removeFilmeEmpty [] = []
removeFilmeEmpty (g@(_,l):ls) | null l = removeFilmeEmpty ls
                              | otherwise = g:removeFilmeEmpty ls

searchGen :: Genero -> [(Filme,Avaliacao)] -> [(Titulo,Avaliacao)]
searchGen g [] = []
searchGen g (((t,_,_,ge,_),p):ls) | g == ge = (t,p):searchGen g ls
                                  | otherwise = searchGen g ls

titAvaliacao :: FilmesAval -> [(Filme,Avaliacao)]
titAvaliacao [] = []
titAvaliacao ((f,a):fs) = (f,mediaAva (somaAval (0,0) a)) : titAvaliacao fs

mediaAva :: (Int,Int) -> Avaliacao
mediaAva (x,y) | x == 0 = NaoVi
               | otherwise = Pontos (div x y)

somaAval :: (Int,Int) -> [Avaliacao] -> (Int,Int)
somaAval (x,y) [] = (x,y)
somaAval (x,y) (NaoVi:ls) = somaAval (x,y) ls
somaAval (x,y) ((Pontos p):ls) = somaAval (x+p,y+1) ls

