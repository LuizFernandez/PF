
--Ficha 5

--Exercicio 1

--a)

any2 :: ( a -> Bool ) -> [a] -> Bool
any2 f [] = False
any2 f (x:xs) | f x = True
              | otherwise = any2 f xs

--b)

zipWith2 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith2 f l [] = []
zipWith2 f (x:xs) (y:ys) = f x y:zipWith2 f xs ys 

--c)

takeWhile2 :: (a->Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (x:xs) | f x = x:takeWhile2 f xs
                    | otherwise = []

--d)

dropWhile2 :: (a->Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f l@(x:xs) | f x = dropWhile2 f xs
                      | otherwise = l

--e)

span2 :: (a-> Bool) -> [a] -> ([a],[a]) 
span2 f [] = ([],[])
span2 f (x:xs) | f x = (x:ds,es)
               | otherwise = (ds,x:es)
            where
                (ds,es) = span2 f xs

--f)

deleteBy2 :: (a -> a -> Bool) -> a -> [a] -> [a] 
deleteBy2 f _ [] = []
deleteBy2 f x (y:ys) | f x y = ys
                     | otherwise = y:deleteBy2 f x ys

--g)

sortOn2 :: Ord b => (a -> b) -> [a] -> [a] 
sortOn2 f [] = []
sortOn2 f (x:xs) = insere x (sortOn2 f xs)
        where 
          insere x [] = [x]
          insere x (y:ys) | f x > f y = y:insere x ys
                          | otherwise = x:y:ys

--Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)

selgrau :: Int -> Polinomio -> Polinomio 
selgrau z  = filter (\(n,g) -> g == z) 

--b)

conta :: Int -> Polinomio -> Int
conta z  = foldr f 0 
    where
    f :: Monomio -> Int -> Int
    f (n,g) i = if g==z then i+1 else i

--c)

grau :: Polinomio -> Int 
grau  = foldr f 0 
    where
        f :: Monomio -> Int -> Int
        f (x,y) i = max y i 

--d)

deriv :: Polinomio -> Polinomio
deriv l = map f (filter g l)
    where
        f :: Monomio -> Monomio
        f (x,y) = (x*fromIntegral y,y-1)
        g :: Monomio -> Bool
        g (x,y) = y>0 

--e)

calcula :: Float -> Polinomio -> Float
calcula x  = foldl (\acc (b,e) -> acc + b*(x^e)) 0 

--f)

simp :: Polinomio -> Polinomio
simp  = filter (\(b,e) -> e/=0) 

--g)

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y)  = map (\(b,e) -> (b*x,e+y)) 

--h)

ordena :: Polinomio -> Polinomio
ordena  = sortOn2 snd 

--i)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):xs) = (sum [bs | (bs,es) <- selgrau e xs] + b,e):normaliza [(bt,et) | (bt,et) <- xs, et /= e]

--j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r

--k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p ps = foldl (\acc x -> soma (mult x ps) acc) [] p

--l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p ps | normaliza p == normaliza ps = True
           | otherwise = False

--Exercicio 3

type Mat a = [[a]]

--a)

dimOK :: Eq a => Mat a -> Bool
dimOK l@(x:xs) | filter (\y -> length x == length y) l == l = True 
               | otherwise = False  

--b)

dimMat :: Eq a => Mat a -> (Int,Int)
dimMat t@(m:ms) | dimOK t = (foldl (\acc2 y -> 1 + acc2) 0 t, foldl (\acc x -> 1 + acc) 0 m)
                | otherwise = error "Operação inválida"

--c)

addMat :: Eq a => Num a => Mat a -> Mat a -> Mat a
addMat [] l = []
addMat t@(m:ms) u@(l:ls) | dimMat t == dimMat u = g m l : addMat ms ls
                         | otherwise = error "Operação inválida"
                        where
                          g [] l = []
                          g m l = (head m + head l) : g (tail m) (tail l)
                       
--d)

transpose :: Mat a -> Mat a
transpose [l] = map (:[]) l
transpose (m:ms)= zipWith2 (++)  (map (:[]) m) (transpose ms)

--e)

multMat :: Eq a => Num a => Mat a -> Mat a -> Mat a
multMat [] l = []
multMat m l | c == ts = map (\x -> formLinha x (transpose l)) m 
            | otherwise = error "Operação inválida"
                 where
                   (t,c) = dimMat m
                   (ts,cs) = dimMat l

multLinha :: Num a => [a] -> [a] -> a
multLinha [] [] = 0
multLinha (x:xs) (l:ls) = (x*l) + multLinha xs ls

formLinha :: Num a => [a] -> [[a]] -> [a]
formLinha l = map (multLinha l) 

--f)

zipWMat :: (Eq a,Eq b) => (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f [] [] = []
zipWMat f u@((a:b):ms) t@((c:d):ls) | dimMat u == dimMat t = (f a c:zipWith2 f b d) : zipWMat f ms ls
                                    | otherwise = error "Operação inválida" 

--g)

triSup :: (Num a, Eq a) => Mat a -> Bool 
triSup m = all (\n -> all ((==) 0 . (!!) (m !! n)) [0..(n - 1)]) [1..(length m - 1)]

--h)

rotateLeft :: Mat a -> Mat a
rotateLeft m = invert (transpose m)

invert :: Mat a -> Mat a
invert [] = []
invert (m:ms) = invert ms ++ [m]