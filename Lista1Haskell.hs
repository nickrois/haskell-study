--Aluno: Nicolas Brambilla Rodrigues
--Matricula:
--Professor: Prof. Dr. Joao Paulo Reus Rodrigues Leite
--Conteudo: Primeira lista de exercicios da materia
--          Laboratorio de Matematica Discreta, ou
--          Programacao Funcional.


--1
cambio :: Float -> Float -> Float
cambio r t = r*t

--2
convtemp :: Float -> Float
convtemp c = (c*1.8) + 32

--3
absoluto :: Int -> Int
absoluto i = if i>0
  then i
  else i*(-1)

--4
funcC :: Int -> Int -> Int
funcC 0 y = 0
funcC x y = if mod x y == 0
  then x + funcC (x-1) y
  else 0 + funcC (x-1) y

--5
fib :: Int -> Int
fib 1=1
fib 2=1
fib n = fib (n-1) + fib (n-2)

--6
muv :: Float -> Float -> Float -> Float
muv v0 v1 t = v0*t + (a*t^2)/2
  where
    a = (v1-v0)/t

--7
primo :: Int -> Bool
primo x = if divisores x == [1,x] then True else False
  where divisores x = [y | y<-[1..x], mod x y==0]

--8
func8 :: [Int] -> [Int] -> [Int]
func8 [] [] = []
func8 lista lista2 = [x | x<-lista, elem x lista2]

--9
func9 :: Int -> Int -> [Int] -> [Int]
func9 n v [] = [n]
func9 n v (h:t)
  |n==1 = v:h:t
  |otherwise = h:func9 (n-1) v t

--10
func10 :: Char -> Char -> [Char] -> [Char]
func10 a v [] = []
func10 a v (h:t) = if a==h then v:func10 a v t
                           else h:func10 a v t

--11
palindromo :: String -> Bool
palindromo s = if s == inverte s then True else False
  where
  inverte [] = []
  inverte (h:t) = inverte t ++ [h]

--12
  --iniciais
iniciais :: [Int] -> Int -> [Int]
iniciais [] n = []
iniciais (h:t) n
  |n==0 = []
  |n==1 = [h]
  |otherwise = h:iniciais t (n-1)

  --finais
finais :: [Int] -> Int -> [Int]
finais [] n = []
finais (h:t) 0 = []
finais (h:t) n
  |tamanho (h:t) == (n+1) = t
  |otherwise= finais t n
    where
      tamanho [] = 0
      tamanho (h:t) = tamanho t + 1

--13
uniao :: [Int] -> [Int] -> [Int]
uniao [] [] = []
uniao [] l2 = l2
uniao l1 [] = l1
uniao (a:b) (h:t)
  |a>h = h:uniao (a:b) t
  |a<h = a:uniao (h:t) b
  |a==h = a:uniao b t

--14
verordem :: [Int] -> Bool
verordem [] = True
verordem (h1:h2:t)
  |h1>h2 = False
  |comp t == 0 = True
  |otherwise = verordem (h2:t)
    where
      comp [] = 0
      comp (h:t) = 1+comp t

--15
primpal :: String -> String
primpal [] = []
primpal (h:t)
  |h==' ' = []
  |otherwise = [h]++primpal t

--16
type Pontotri = (Float, Float, Float)

distponto :: Pontotri -> Pontotri -> Float
distponto (a1, a2, a3) (b1, b2, b3) = sqrt((a1-b1)^2 + (a2-b2)^2 + (a3-b3)^2)

--17
type Data = (Int, Int, Int)

verdata :: Data -> Bool
verdata (a, b, c)
  |a<=31 && a>0 && b<=12 && b>0 = verifica a b c --ano sem parametros porque pode ser muito grande, pequeno ou negativo...
  |otherwise = False
    where
      verifica a b c
        |b==2 && a>29 = False --verifica fevereiro
        |b==2 && a==29 && mod c 4 /=0 = False --verifica ano bissexto fevereiro
        |(b==4 || b==6 || b==9 || b==11) && a>30 = False --verifica meses ate 30 dias
        |otherwise = True

--18
type Triangulo = (Float, Float, Float)

tipotri :: Triangulo -> Float
tipotri (a, b, c)
  |a==b && a==c = 3
  |(a==b || a==c) || (b==c || b==a) = 2
  |otherwise = 1
