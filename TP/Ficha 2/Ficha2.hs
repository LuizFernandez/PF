
import Data.Char

--Ficha 2

--Exercicio 1

--a)

funA :: [Double] -> Double
funA [] = 0 
funA (a:as) = a^2 + (funA as)

{-
funA [2,3,5,1]=
	=> 2^2 + (funA [3,5,1])
	=> 4 + 3^2 + (funA [5,1])
	=> 4 + 9 + 5^2 + (funA [1])
	=> 13 + 25 + 1^2 + (funA [])
	=> 38 + 1 + 0
	=> 39
-}

--b) 

funB :: [Int] -> [Int]
funB [] = []
funB (b:bs) = if (mod b 2) == 0 then b:(funB bs)
                                else (funB bs)

{-
funB [8,5,12]=
	=> 8:(funB [5,12])
	=> 8:(funB [12])
	=> 8:12:(funB[])
	=> [8,12]
-}

--c)

funC :: [a] -> [a]
funC [] = []
funC [x] = []
funC (c:cs:ch) = funC ch

{-
funC [1,2,3,4,5]=
	=> funC [3,4,5]
	=> funC [5]
	=> []
-}

--d)

funD :: String -> String
funD l = g [] l

g :: String-> String -> String
g l [] = l
g l (x:xs) = g (x:l) xs

{-
funD "otrec"=
	=> g [] " otrec"
	=> g ['o'] "trec"
	=> g ['t','o'] "rec"
	=> g ['r','t','o'] "ec"
	=> g ['e','r','t','o'] "c"
	=> g ['c','e','r','t','o'] []
	=> "certo"
-}

--Exercicio 2

--a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros a = [x*2 | x <- a]

--b)

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (b:bs) | x == b = 1 + (numOcorre x bs) 
                   | otherwise = numOcorre x bs

--c)

positivos :: [Int] -> Bool
positivos [] = True 
positivos (c:cs) | c > 0 = positivos cs 
                 | otherwise = False

--d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (d:ds) | d < 0 = soPos ds
             | otherwise = d : (soPos ds)

--e)

somaNeg :: [Int] -> Int
somaNeg l = somaNegAcc 0 l
        where
            somaNegAcc n [] = n
            somaNegAcc n (x:xs) | x < 0 = somaNegAcc (n+x) xs
                                | otherwise = somaNegAcc n xs

--f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt f | length f < 3 = f
          | otherwise = drop ((length f) -3) f

--g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((g,h):gs) = h:(segundos gs)

--h)

nosPrimeiro :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiro _ []                     = False
nosPrimeiro x ((y,z):hs) | x == y    = True
                         | otherwise = nosPrimeiro x hs 

--i) 

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos l                                      = sumTriplosAcc (0,0,0) l
           where
            sumTriplosAcc (x,y,z) []              = (x,y,z)
            sumTriplosAcc (x,y,z) ((xs,ys,zs):ls) = sumTriplosAcc ((x+xs),(y+ys),(z+zs)) ls

--Exercicio 3

--a)

soDigitos :: [Char] -> [Char]
soDigitos l = [digit | digit <- l, elem digit ['0'..'9']]

--b)

minusculas :: [Char] -> Int
minusculas l = foldl (\acc x -> if elem x ['a'..'z'] then acc + 1 
                                                     else acc
                     ) 0 l

--c)

nums :: String -> [Int]
nums l = [ord n - ord '0' | n <- l, elem n ['0'..'9']]

--Exercicio 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((b,e):as) | n == e = 1 + (conta n as)
                   | otherwise = conta n as

--b)

grau :: Polinomio -> Int
grau [(b,e)] = e
grau ((b,e):bs) | e > grau bs = e
                | otherwise = grau bs

--c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n c = [(b,e) | (b,e) <- c, e == n]

--d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((b,e):ds) | e > 0 = (b*(fromIntegral e),e-1):(deriv ds)
                 | otherwise = deriv ds

--e)

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((b,e):es) = (b*(n^e)) + (calcula n es) 

--f)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((b,e):fs) | b == 0 = simp fs
                | otherwise = (b,e):(simp fs)

--g)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (b,e) ((bs,es):gs) | b == bs = (b,e+es):(mult (b,e) gs)
                        | e == es = (b*bs,e):(mult (b,e) gs)
                        | otherwise = (b*bs,e+es):(mult (b,e) gs)

--h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(b,e)] = [(b,e)]
normaliza ((b,e):(bs,es):hs) = if e == es then normaliza (((b+bs),e):hs)
                                          else if conta e hs == 0 then (b,e):normaliza ((bs,es):hs)
                                                                  else normaliza ((b,e):hs) ++ [(bs,es)]

--i)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

--j)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (j:js) p = soma (mult j p) (produto js p)

--k)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((b,e):ks) = ordena (maisAltos ks) ++ [(b,e)] ++ ordena (maisBaixos ks)
                  where
                    maisBaixos [] = []
                    maisBaixos ((bs,es):k) = if (es < e || (es == e && bs < b)) then (bs,es):maisBaixos k else maisBaixos k
                    maisAltos [] = []
                    maisAltos ((bs,es):k) = if (es > e || (es == e && bs >= b)) then (bs,es):maisAltos k else maisAltos k

--l)

equiv :: Polinomio -> Polinomio -> Bool
equiv l ls = ordena (normaliza l) == ordena (normaliza ls)