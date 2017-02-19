--Tipo palabra y lista de palabras
type Palabra = [Char]
type Destino = Palabra
type ListaPalabras = [Palabra]

--Funcion unicos: dada una lista devuelve la misma sin elementos repetidos
--debido a que unic devuelve el segundo de los elementos repetidos,
--utilizamos dos reverses (La eficiencia no es nuestra mayor preocupación)
--para devolver el primero.
unicos [] = []
unicos xs = reverse (unic (reverse xs))

unic :: Eq a => [a] -> [a]
unic []       = []
unic (x:xs)
    | elem x xs = unic xs
    | otherwise = x : unic xs

--Funcion conjuntos: para cada elemento de la lista nos devuelve una pareja
--con ese elemento y el resto de la lista, se utiliza en las permutaciones
--y en algunas permutaciones
conjuntos :: Eq a => [a] -> [(a,[a])]
conjuntos xs = conjuntos' xs (length xs)
    where
        conjuntos' [] n       = []
        conjuntos' (x:xs) n
            | n < 2      = [(x,xs)]
            | otherwise  = (x,xs) : (conjuntos' (rotar (x:xs)) (n-1))

--Funcion permutaciones: dada una lista de caracteres, devuleve una lista con
--las posibles permutaciones. Idea original de Koen  Claessen:
--https://mail.haskell.org/pipermail/haskell-cafe/2002-June/003122.html
permutaciones :: Palabra -> ListaPalabras
permutaciones [] = []
permutaciones (x:xs)
    | xs == []   = [[x]]
    | otherwise  = [ y:zs | (y,ys) <- conjuntos (x:xs), zs <- permutaciones ys ]

rotar :: [a] -> [a]
rotar (x:xs) = reverse (x : (reverse xs))
--rotar (x:xs) = xs ++ [x]

--Funcion desplazar: mapea la funcion desplaza a todos los elementos de la lista
desplazar :: Palabra -> Palabra
desplazar = map desplaza

--Funcion desplaza: toma el valor numérico de un caracter y devuelve el siguiente (ciclando en la z)
--de momento falta la Ñ, se añadirá más tarde si se considera necesario
desplaza :: Char -> Char
desplaza x = if (fromEnum x >= fromEnum 'A' && fromEnum x <= fromEnum 'z') then
                                                if(fromEnum x == fromEnum 'z') then 'a'
                                                else if(fromEnum x == fromEnum 'Z') then 'A'
                                                     else (toEnum ((fromEnum x)+1)) :: Char
                                            else error "Hay un parásito en la palabra"

--Funcion intersecar: dadas dos listas devuelve una con los elementos que pertenecen a ambas
intersecar :: Palabra -> Palabra -> Palabra
intersecar xs ys = unicos [x | x <- xs, y <- ys, x==y]

--Funcion restar: dadas dos listas devuelve los elementos de la primera que no estan en la segunda
--restar :: Palabra -> Palabra -> Palabra
restar :: Eq a => [a] -> [a] -> [a]
restar [] ys   = []
restar xs []   = xs
restar (x:xs) ys
    | elem x ys = restar xs ys
    | otherwise = x : (restar xs ys)

--Funcion concatenar: sin mas
concatenar :: Palabra -> Palabra -> Palabra
concatenar xs ys = xs ++ ys

--Funcion transformaciones: distintos tipos de transformacion. Devuleve una lista de parejas:
--fst es la lista una vez hemos aplicado la transformacion,
--snd es un String que describe la transformacion que hemos hecho
transformaciones :: Palabra -> ListaPalabras -> [(ListaPalabras,ListaPalabras)]
transformaciones _ []       = []
transformaciones "P" (x:xs) = transf' (x:xs) (length (x:xs))
  where
    transf' _ 0      = []
    transf' (x:xs) n = (xs++(restar (unicos $ permutaciones x) [x]),["Permutacion "++x]): (transf' (rotar (x:xs)) (n-1))
transformaciones "D" (x:xs) = transf' (x:xs) (length (x:xs))
  where
    transf' _ 0      = []
    transf' (x:xs) n = (xs++[desplazar x],["Desplazamiento "++x]): (transf' (rotar (x:xs)) (n-1))
transformaciones "C" (x:xs) = transf' (x:xs) (length (x:xs))
  where
    transf' _ 0      = []
    transf' (x:xs) n = [ ([(concatenar x y)]++ys,["Concatenacion "++x++" "++y]) | (y,ys) <- conjuntos xs] ++ (transf' (rotar (x:xs)) (n-1))
transformaciones "I" (x:xs) = transf' (x:xs) (length (x:xs))
  where
    transf' _ 0      = []
    transf' (x:xs) n = [ ([(intersecar x y)]++[y]++ys,["Interseccion "++x++" "++y]) | (y,ys) <- conjuntos xs] ++ (transf' (rotar (x:xs)) (n-1))
transformaciones "R" (x:xs) = transf' (x:xs) (length (x:xs))
  where
    transf' _ 0      = []
    transf' (x:xs) n = [ ([(restar x y)]++[y]++ys,["Resta "++x++" "++y]) | (y,ys) <- conjuntos xs] ++ (transf' (rotar (x:xs)) (n-1))
transformaciones _ _        = []

--Funcion solucion: aproximacion al algoritmo de backtracking, dado un estado verificamos si es final:
--si es estado final pero no valido (hemos alcanzado la profundidad límite) devolvemos lista vacía
--si es válido pero no final devolvemos las soluciones de llamar a vuelta atrás con los hijos del nodo actual
--(todas las transformaciones posibles para cada palabra de la lista) y un nivel más de profundidad
--si es estado final y válido, devolvemos la lista de transformaciones que nos ha llevado hasta aquí.
--solucion :: ListaPalabras -> Palabra -> Int -> ListaPalabras
solucion ls d k = vueltaAtras ls d (k+1) []
    where
        vueltaAtras _ _ 0 _ = []
        vueltaAtras ls d k ys
            | any (==d) ls = [ys]
            | otherwise    = concat [ (vueltaAtras (fst x) d (k-1) (ys++(snd x))) | y <- ["C","D","P","I","R"], x <- (transformaciones y ls)]

--Funcion pregunta: interaccion IO que devuleve la respuesta a una pregunta
pregunta :: String -> IO String
pregunta s = do
                putStrLn s
                x <- getLine
                return x

--Funcion seguimos: Interaccion IO que nos devuleve al inicio del main si queremos seguir
seguimos :: IO ()
seguimos = do putStrLn "¿Quieres seguir (s/n)?"
              seguir <- getChar
              case seguir of
                's' -> do putStrLn ""
                          putStrLn "Bien!"
                          main
                'n' -> do putStrLn ""
                          putStrLn "Adios!"
                _   -> do seguimos


main = do dest <- pregunta "Palabra destino:"
          maxim <- pregunta "Nivel de profundidad máxima:"
          putStrLn "Escribe las palabras a transformar con espacios sin comas:"
          pal <- getLine
          let prof = read maxim :: Int
              lista = words pal
              in print $ solucion lista dest prof
          seguimos
