soma :: Int -> Int -> Int
soma x y = x + y

perimetro :: Float -> Float -> Float
perimetro w h = (2*w) + (2*h)

areatriangulo :: Float -> Float -> Float
areatriangulo b h = b*h/2

velmedia :: Float -> Float -> Float
velmedia s t = s/t

difvelmedia :: Float -> Float -> Float -> Float
difvelmedia t1 t2 s = abs((velmedia s t1)-(velmedia s t2))

convdollar :: Float -> Float -> Float
convdollar r t = r*t

convtemp :: Float -> Float
convtemp c = (c*1.8) + 32

ispar :: Int -> Bool
ispar i = if mod i 2 == 0
   then True
   else False

ismultiplo :: Int -> Int -> Bool
ismultiplo i j = if mod i j == 0
  then True
  else False

absoluto :: Int -> Int
absoluto i = if i>0
  then i
  else i*(-1)

menorchar :: Char -> Char -> Char
menorchar a b = if 'a'>'b'
  then 'b'
  else 'a'

menornum :: Float -> Float -> Float -> Float
menornum a b c | a<b && a<c = a
               | b<a && b<c = b
               | otherwise = c

funcny :: Float -> Float -> Float
funcny n y | y==0 = 1
           | y>=1 && y<= 5 = n
           | y>5 = n^5

ackerman :: Float -> Float -> Float
ackerman m n
  |m==0 = n+1
  |m>0 && n==0 = ackerman (m-1) 1
  |m>0 && n>0 = ackerman (m-1) (ackerman (m) (n-1))

tipotri :: Float -> Float -> Float -> Float
tipotri a b c
  |a==b && a==c = 3
  |(a==b || a==c) || (b==c || b==a) = 2
  |otherwise = 1

func12 :: Float -> Float -> Float
func12 a b
  |a<b = -(a*b)
  |a==b = 0
  |a>b = a*b

func13 :: Int -> Int -> Int
func13 0 y = 0
func13 x y = if mod x y == 0
  then x + func13 (x-1) y
  else 0 + func13 (x-1) y

--divisores :: Int -> [Int]
--divisores x = [y | y<-[1..x], mod x y==0]

--primo :: Int -> Bool
--primo x = divisores x == [1,x]

primorec :: Int -> Bool
primorec x = if x<2 then False
             else verifica x (x-1)

  where
    verifica x 1 = True
    verifica x y
      |mod x y == 0 = False
      |otherwise = verifica x (y-1)


primore :: Int -> Bool
primore 0 = False
primore 1 = False
primore x = verifica x (x-1)
  where
    verifica x 1 = True
    verifica x y
      |mod x y == 0 = False
      |otherwise = verifica x (y-1)

fib :: Int -> Int
fib 1=1
fib 2=1
fib n = fib (n-1) + fib (n-2)

muv :: Float -> Float -> Float -> Float
muv v0 v1 t = v0*t + (a*t^2)/2
  where
    a = (v1-v0)/t

muvlet :: Float -> Float -> Float -> Float
muvlet v0 v1 t = let a=(v1-v0)/t
                 in v0*t+(a*t^2)/2

soma17 :: Int -> Int -> Int
soma17 m 1 = m+1
soma17 m n = soma17 (m+1) (n-1)

primo :: Int -> Bool
primo x = if x<2 then False
          else verifica x (x-1)
        where
          verifica x 1 = True
          verifica x y
            |mod x y == 0 = False
            |otherwise = verifica x (y-1)

pertence :: Int -> [Int] -> Bool
pertence x [] = False
pertence x (h:t)
    |x==h = True
    |otherwise = pertence x t

func18 :: [Int] -> [Int] -> [Int]
func18 [] [] = []
func18 lista lista2 = [x | x<-lista, elem x lista2]

func19 :: Int -> [Int] -> Int
func19 n [] = -1
func19 n (h:t)
  |n==1 = h
  |otherwise = func19 (n-1) t

func20 :: Int -> Int -> [Int] -> [Int]
func20 n v [] = [n]
func20 n v (h:t)
  |n==1 = v:h:t
  |otherwise = h:func20 (n-1) v t

func21 :: Char -> Char -> [Char] -> [Char]
func21 a v [] = []
func21 a v (h:t) = if a==h then v:func21 a v t
                 else h:func21 a v t

inverte :: String -> String
inverte [] = []
inverte (h:t) = inverte t ++ [h]

palindromo :: String -> Bool
palindromo s = if s == inverte s then True else False

iniciais :: [Int] -> Int -> [Int]
iniciais [] n = []
iniciais (h:t) n
  |n==0 = []
  |n==1 = [h]
  |otherwise = h:iniciais t (n-1)

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (h:t) = tamanho t + 1

finais :: [Int] -> Int -> [Int]
finais [] n = []
finais (h:t) 0 = []
finais (h:t) n
  |tamanho (h:t) == (n+1) = t
  |otherwise= finais t n

prefixo :: [Int] -> [Int] -> Bool
prefixo [] [] = True
prefixo [] (a:b) = True
prefixo (h:t) [] = False
prefixo (h:t) (a:b)
  |h==a = prefixo t b
  |otherwise=False

sozinhos :: [[Int]] -> [[Int]]
sozinhos [] = []
sozinhos (h:t)
  |tamanho h == 1 = h:sozinhos t
  |otherwise = sozinhos t

positivos :: [Int] -> [Int]
positivos [] = []
positivos (h:t)
  |h<=0 = positivos t
  |otherwise = h:positivos t

pares :: [Int] -> [Int]
pares [] = []
pares (h:t)
  |mod h 2 == 0 = h:pares t
  |otherwise = pares t

uniao :: [Int] -> [Int] -> [Int]
uniao [] [] = []
uniao [] l2 = l2
uniao l1 [] = l1
uniao (a:b) (h:t)
  |a>h = h:uniao (a:b) t
  |a<h = a:uniao (h:t) b
  |a==h = a:uniao b t

inters :: [Int] -> [Int] -> [Int]
inters [] [] = []
inters [] l2 = l2
inters l1 [] = l1
inters (a:b) (c:d)
  |a>c = inters (c:d) b
  |a==c = a:inters b d
  |a<c = inters (a:b) d

average :: [Float] -> Float
average [] = 0
average (h:t) = if tam (h:t)/=0 then (h+ average t)
                else 
