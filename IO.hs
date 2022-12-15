type Tri = (Float,Float,Float)

main :: IO ()
main = do
  menu

menu :: IO ()
menu = do
  putStrLn "1 - Lista de numeros multiplos de 7."
  putStrLn "2 - Consoantes de uma frase."
  putStrLn "3 - Area de um triangulo."
  putStrLn "4 - Sair."
  op <- readLn
  funcoes op
  if op==4 then return () else menu

funcoes :: Int -> IO ()
funcoes op = do
  if op==1 then do
    putStrLn "Multiplos: "
    l <- multiplos
    imprime_lista l
  else if op==2 then do
    putStrLn "Digite frase: "
    frase <- getLine
    putStrLn ("Consoantes: " ++ consoantes frase)
  else if op==3 then do
    l <- lados
    putStrLn ("Area: "++ show (area_triangulo l))
  else if op==4 then do
    putStrLn "Saindo..."
  else do
    putStrLn "Funcao invalida."

multiplos :: IO [Int]
multiplos = do
  return [x | x <- [1..100], mod x 7 == 0]

imprime_lista :: [Int] -> IO ()
imprime_lista [] = do putStrLn ""
imprime_lista (h:t) = do
  putStrLn ((show h) ++ " ")
  imprime_lista t


consoantes :: [Char] -> [Char]
consoantes [] = []
consoantes (h:t)
  |elem h controle = [h]++ consoantes t
  |otherwise = consoantes t
    where
      controle = "BCDFGHJKLMNPQRSTVWXYZbcdfghjklmnpqrstvwxyz"

lados :: IO Tri
lados = do
  putStrLn "Lado a: "
  a <- readLn
  putStrLn "Lado b: "
  b <- readLn
  putStrLn "Lado c: "
  c <- readLn
  return (a,b,c)

area_triangulo :: Tri -> Float
area_triangulo (a,b,c) = sqrt(s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2
