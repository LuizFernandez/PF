
--Ficha 6

--Exercicio 1

data BTree a = Empty
             | Node a (BTree a) (BTree a)
                deriving Show

--a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node x l r) = 1 + max (altura l) (altura r)

--b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x l r) = 1 + (contaNodos l) + (contaNodos r)

--c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x l r) = 0 + folhas l + folhas r

--d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune x (Node y l r) | x <= 0 = Empty
                     | otherwise = (Node y (prune (x-1) l) ( prune (x-1) r)) 

--e)

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path l Empty = []
path (x:xs) (Node y l r) | x==False = y:path xs l
                         | otherwise = y:path xs r
                
--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x l r) = (Node x (mirror r) (mirror l)) 

--g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f _ Empty = Empty 
zipWithBT f Empty _ = Empty
zipWithBT f (Node x l r) (Node y e d) = Node (f x y) (zipWithBT f l e) (zipWithBT f r d) 

--h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) l r) = (Node x la ra, Node y lb rb, Node z lc rc)
    where
    (la,lb,lc) = unzipBT l
    (ra,rb,rc) = unzipBT r

--Exercicio 2

procura :: Ord a => a -> BTree a -> Bool
procura x Empty = False
procura x (Node y l r) 
    | x == y = True
    | x > y = procura x r
    | x < y = procura x l
--a)

minimo :: Ord a => BTree a -> a
minimo (Node x Empty r) = x
minimo (Node x l _) = minimo l

--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty Empty) = Empty
semMinimo (Node x l r) = (Node x (semMinimo l) r)

--c)

minimo' :: Ord a => BTree a -> (a,BTree a)
minimo' (Node x Empty r) = (x,r)
minimo' (Node x l r) = (m,Node x l' r) 
    where
    (m,l') = minimo' l

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin t = (minimo t,semMinimo t)

--d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node y l r) | x == y = Empty
                      | x > y = (Node y l (remove x r))
                      | x < y = (Node y (remove x l) r)

--Exercicio 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
                    deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

turma1 :: Turma
turma1 = (Node (15,"Luis",ORD, Aprov 14) (Node (12,"Joana",MEL,Faltou)(Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty)
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                         (Node (20,"Pedro",TE,Aprov 10) Empty 
                                                                        (Node (25,"Sofia",ORD,Aprov 20)(Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))
--a)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (n,_,_,_) e d) | x == n = True
                               | x > n = inscNum x d
                               | x < n = inscNum x e

--b)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) l r) = n == nom || inscNome n l || inscNome n r

--c)
                                              
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,_) e d) = (case reg of TE -> [(num,nom)]; otherwise -> []) ++ (trabEst e) ++ (trabEst d)

--d)

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing 
nota n (Node (num,_,_,clas) e d) | n == num = Just clas
                                 | n < num = nota n e
                                 | otherwise = nota n d

--e)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = (numFaltas t) / (tamTurma t)
           where
           numFaltas Empty = 0
           numFaltas (Node (_,_,_,clas) e d) = (case clas of Faltou -> 1; otherwise -> 0) + (numFaltas e) + (numFaltas d)
           tamTurma Empty = 0
           tamTurma (Node e l r) = 1 + (tamTurma l) + (tamTurma r)

--f)

mediaAprov :: Turma -> Float 
mediaAprov Empty = 0
mediaAprov t = (sumNotas t) / (numNotas t)
          where
            sumNotas Empty = 0
            sumNotas (Node (_,_,_,Aprov n) e d) = (fromIntegral n) + (sumNotas e) + (sumNotas d)
            sumNotas (Node l e d) = (sumNotas e) + (sumNotas d)
            numNotas Empty = 0
            numNotas (Node (_,_,_,clas) e d) = (case clas of Aprov n -> 1; otherwise -> 0) + (numNotas e) + (numNotas d)

--g)

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv t = a/b
        where
            (a,b) = aux t
            aux :: Turma -> (Float,Float)
            aux Empty = (0,0)
            aux (Node (_,_,_,clas) l r) = (case clas of Aprov n -> (x+1,y); Rep -> (x,y+1); otherwise -> (x,y))
                where
                    (x,y) = (c+e,d+f) 
                    (c,d) = aux l
                    (e,f) = aux r