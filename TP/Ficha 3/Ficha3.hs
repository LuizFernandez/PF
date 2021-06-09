
--Ficha 3


--Exercicio 1

data Hora = H Int Int
        deriving (Show)

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--a)

horaValida :: Hora -> Bool
horaValida (H h m) = if 0 <= h && h < 24 && 0 <= m && m < 60 then True else False 

etapaValida :: Etapa -> Bool
etapaValida (H h1 m1, H h2 m2)  | horaValida (H h1 m1) && horaValida (H h2 m2) && h1<h2 = True 
                                | horaValida (H h1 m1) && horaValida (H h2 m2) && h1==h2 && m1<m2 = True
                                | otherwise = False

--b)

viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [x] = True
viagemValida (x:y:xs) = etapaValida x && etapasConsecutivas x y && viagemValida (y:xs)

etapasConsecutivas :: Etapa -> Etapa -> Bool
etapasConsecutivas (_,chegada) (partida,_) = etapaValida (chegada,partida)

--c)

partChega :: Viagem -> (Hora,Hora)
partChega [(h1,h2)]=(h1,h2)
partChega [(h1,h2),(h3,h4)] = (h1,h4)
partChega ((h1,h2):(h3,h4):t) = partChega((h1,h2):t)

--d)

etapaMin :: Etapa -> Int
etapaMin ((H h1 m1),(H h2 m2)) = (h2*60 + m2) - (h1*60 + m1)

tempViagem :: Viagem -> Int
tempViagem [] = 0 
tempViagem ((h1,h2):t) = etapaMin (h1,h2) + tempViagem t

--e)

etapaEspera :: Etapa -> Etapa -> Int
etapaEspera (_,(h1)) (h2,_) = etapaMin (h1,h2)

tempEspera :: Viagem -> Int
tempEspera [] = 0
tempEspera ((h,h1):(h2,h3):t) = (etapaEspera (h,h1) (h2,h3)) + (tempEspera ((h2,h3):t))

--f)

viagemTotal :: Viagem -> Int
viagemTotal [] = 0
viagemTotal v = (tempViagem v) + (tempEspera v)

--Exercicio 2

type Poligonal = [Ponto]
 
data Ponto = Cartesiano Double Double | Polar Double Double
           deriving (Show,Eq)

polarToCart :: Ponto -> Ponto
polarToCart (Polar x y) = Cartesiano ((cos y)*x) ((sin y)*x)
polarToCart (Cartesiano x y) = Cartesiano x y

--a)

compPoli :: Poligonal -> Double
compPoli [l] = 0
compPoli (p1:p2:t) = (dist (polarToCart p1) (polarToCart p2)) + compPoli (p2:t)

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

--b)

poliFechado :: Poligonal -> Bool
poliFechado (x:xs) | elem x xs || elem (polarToCart x) xs = True
                   | otherwise = False

--c)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:p) = (Triangulo p1 p2 p3) : triangula (p1:p3:p)

--d)

arePol :: Poligonal -> Double
arePol p = sum (map (\fig -> area fig) (triangula p))

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist p1 p2
                                b = dist p2 p3
                                c = dist p3 p1
                                s = (a+b+c)/2
                           in sqrt(s*(s-a)*(s-b)*(s-c))
area (Circulo p1 r) = (r^2) * pi
area (Rectangulo (Cartesiano x xs) (Cartesiano y ys)) = a*b
                                where
                                        a = abs(x-y)
                                        b = abs(ys-xs)

--e)

mover :: Poligonal -> Ponto -> Poligonal
mover p ps = (ps:p)

--f)

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1,(Cartesiano a b)] = [p1,(Cartesiano (a*z) (b*z))]
zoom z (p1:(Cartesiano a b):ps) = (p1:(zoom z (p2:ps))) 
      where
        p2 = (Cartesiano (a*z) (b*z))

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
acrescEmail n l a = a ++ [(n, [Email l])]

--b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(n,c)] = if nome == n then Just (map (\x -> case x of Email e -> e) c) else Nothing
verEmails nome ((n,c):xs) = if nome == n then verEmails nome [(n,c)] else verEmails nome xs

--c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:cs) = case c of
                      Casa x -> x : consTelefs cs 
                      Trab x -> x : consTelefs cs
                      Tlm x -> x : consTelefs cs
                      otherwise -> consTelefs cs

--d)

casa :: Nome -> Agenda -> Maybe Integer 
casa nome [(n,(c:cs))] = if nome == n then case c of Casa x -> Just x
                                                     otherwise -> casa nome [(n,cs)]
                                      else Nothing
casa nome ((n,c):xs) = if nome == n then casa nome [(n,c)] else casa nome xs

--Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
       deriving (Show)

type TabDN = [(Nome,Data)]

--a)

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):xs) | nome == n = Just d
                        | otherwise = procura n xs

--b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade d@(D ds m a) nome ((n,D dx mx ax):xs) = if nome == n then if m > mx || m == mx && ds > dx 
                                                                then Just (ax - a)
                                                                else Just ((ax -a) -1)
                                                           else idade d nome xs

--c)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D dx mx ax) = a < ax || (a == ax && (m < mx || (m == mx && d < dx )))

--d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):xs) = insere (n,d) (ordena xs)
        where
          insere (n,d) [] = [(n,d)]
          insere (n,d) ((ns,ds):xs) | anterior ds d = (ns,ds) : insere (n,d) xs
                                    | otherwise = (n,d):(ns,ds):xs

--e)

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) t = (n,idade) : porIdade (D d m a) ts
              where
                (n,D dx mx ax):ts = ordena t
                idade = if m > mx || mx == m && d > dx then (a-ax) else ((a-ax) -1)

--Exercicio 5

data Movimento = Credito Float | Debito Float
            deriving (Show)

data Extracto = Ext Float [(Data, String, Movimento)]
          deriving (Show)

--a)

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext x []) _ = []
extValor (Ext x ((_,_,mov):ls)) v = case mov of
                               Credito n -> if n>=v then mov : extValor (Ext x ls) v else extValor (Ext x ls) v
                               Debito n -> if n>=v then mov : extValor (Ext x ls) v else extValor (Ext x ls) v

--b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext x []) _ = []
filtro (Ext x ((dat,desc,mov):ls)) s = if elem desc s then (dat,mov):filtro (Ext x ls) s else filtro (Ext x ls) s

--c)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext x lm) = foldl (\(n,d) (_,_,m) -> case m of 
                                            Credito l -> ((n+l),d)
                                            Debito l -> (n,(d+l))) (0,0) lm

--d)

saldo :: Extracto -> Float
saldo  (Ext x lm) = foldl (\acc (_,_,m) -> case m of 
                                           Credito n -> (acc + n)
                                           Debito n -> (acc - n)) x lm