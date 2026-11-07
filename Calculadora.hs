module Calculadora where
import Data.List (transpose)
import Data.Ratio

verificarDim :: [[a]] -> Bool
verificarDim [] = True
verificarDim (fila:filas) =
    all (\otra -> length otra == length fila) filas

matrizCuadrada :: [[a]] -> Bool
matrizCuadrada [] = False
matrizCuadrada (fila:filas) =
    verificarDim (fila:filas) &&
    length (fila:filas) == length fila

validarMatriz :: [[a]] -> Either String ()
validarMatriz [] =
    Left "La matriz no puede estar vacia"

validarMatriz (fila:filas)
    | not (verificarDim (fila:filas)) =
        Left "Las filas tienen longitudes diferentes"
    | null fila =
        Left "La matriz no puede tener filas vacias"
    | otherwise =
        Right ()

matrizDim :: [[a]] -> Either String Int
matrizDim mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right () ->
            if matrizCuadrada mss
            then Right (length mss)
            else Left "La matriz no es cuadrada"

submatriz :: Int -> Int -> [[a]] -> Either String [[a]]
submatriz fila columna mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right () -> construirSubmatriz fila columna mss
    where
        construirSubmatriz fila columna (primeraFila:filas)
            | fila < 0 || fila >= length (primeraFila:filas) =
                Left "Indice de fila fuera de rango"
            | columna < 0 || columna >= length primeraFila =
                Left "Indice de columna fuera de rango"
            | otherwise =
                Right
                    [ [ elemento | (j, elemento) <- zip [0..] elementos, j /= columna]
                    | (i, elementos) <- zip [0..] (primeraFila:filas), i /= fila]
        construirSubmatriz _ _ [] =
            Left "La matriz no puede estar vacia"

determinante :: [[Rational]] -> Either String Rational
determinante [] =
    Left "La matriz no puede estar vacia"

determinante mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right ()
            | not (matrizCuadrada mss) ->
                Left "La matriz no es cuadrada"
            | otherwise ->
                determinanteValida mss

determinanteValida :: [[Rational]] -> Either String Rational

determinanteValida [] = Right 1

determinanteValida [[x]] = Right x

determinanteValida (fila:filas) =
    sum <$> mapM termino (zip [0..] fila)
  where
    mss = fila:filas

    termino (j, elemento) = do
        menor <- submatriz 0 j mss
        detMenor <- determinante menor

        let producto = elemento * detMenor

        if even j
            then Right producto
            else Right (negate producto)

traza :: [[Rational]] -> Either String Rational
traza mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right ()
            | not (matrizCuadrada mss) ->
                Left "La matriz no es cuadrada"
            | otherwise ->
                Right (sum [elemento | (i, fila) <- zip [0..] mss,
                                       (j, elemento) <- zip [0..] fila,
                                       i == j])

cofactor :: Int -> Int -> [[Rational]] -> Either String Rational
cofactor fila columna mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right ()
            | not (matrizCuadrada mss) ->
                Left "La matriz no es cuadrada"
            | fila < 0 || fila >= length mss ->
                Left "Indice de fila fuera de rango"
            | columna < 0 || columna >= length mss ->
                Left "Indice de columna fuera de rango"
            | otherwise -> do
                menor <- submatriz fila columna mss

                detMenor <-
                    if null menor
                    then Right 1
                    else determinante menor

                let signo = if even (fila + columna)
                            then 1
                            else -1

                Right (fromIntegral signo * detMenor)

matrizCofactores :: [[Rational]] -> Either String [[Rational]]
matrizCofactores mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right ()
            | not (matrizCuadrada mss) ->
                Left "La matriz no es cuadrada"
            | otherwise ->
                mapM (\i -> mapM (\j -> cofactor i j mss)
                                  [0 .. length mss - 1])
                     [0 .. length mss - 1]

matrizAdjunta :: [[Rational]] -> Either String [[Rational]]
matrizAdjunta mss =
    fmap transpose (matrizCofactores mss)

verificarInvertible :: [[Rational]] -> Either String Bool
verificarInvertible mss = do
    det <- determinante mss
    Right (det /= 0)

inversa :: [[Rational]] -> Either String [[Rational]]
inversa mss = do
    det <- determinante mss

    if det == 0
        then Left "La matriz no es invertible: determinante cero"
        else do
            adj <- matrizAdjunta mss
            Right (map (map (/ det)) adj)

matrizIdentidad :: Int -> Either String [[Rational]]
matrizIdentidad n
    | n <= 0 = Left "La dimension debe ser positiva"
    | otherwise =
        Right
            [ [ if i == j then 1 else 0
              | j <- [0 .. n - 1]
              ]
            | i <- [0 .. n - 1]
            ]

multiplicarM :: [[Rational]] -> [[Rational]] -> Either String [[Rational]]
multiplicarM m1 m2 =
    case validarMatriz m1 of
        Left mensaje -> Left mensaje
        Right () ->
            case validarMatriz m2 of
                Left mensaje -> Left mensaje
                Right () ->
                    case m1 of
                        [] -> Left "La primera matriz está vacía"
                        (fila:_) ->
                            if length fila /= length m2
                            then Left "Las dimensiones no permiten multiplicar las matrices"
                            else
                                Right
                                    [ [ sum (zipWith (*) fila columna)
                                      | columna <- transpose m2
                                      ]
                                    | fila <- m1
                                    ]

verificarInversa :: [[Rational]] -> Either String Bool
verificarInversa mss = do
    inv <- inversa mss
    producto <- multiplicarM mss inv
    identidad <- matrizIdentidad (length mss)
    Right (producto == identidad)

esSimetrica :: [[Rational]] -> Either String Bool
esSimetrica mss =
    case validarMatriz mss of
        Left mensaje -> Left mensaje
        Right ()
            | not (matrizCuadrada mss) ->
                Left "La matriz no es cuadrada"
            | otherwise ->
                Right (mss == transpose mss)

sumarMatrices :: [[Rational]] -> [[Rational]] -> Either String [[Rational]]
sumarMatrices [] _ = Left "Las matrices no pueden estar vacias"
sumarMatrices _ [] = Left "Las matrices no pueden estar vacias"
sumarMatrices m1@(f1:_) m2@(f2:_) = do
    validarMatriz m1
    validarMatriz m2
    sumarSiDimensionesIguales m1 m2 f1 f2

sumarSiDimensionesIguales :: [[Rational]] -> [[Rational]] -> [Rational] -> [Rational] -> Either String [[Rational]]
sumarSiDimensionesIguales m1 m2 f1 f2
    | length m1 /= length m2       = Left "Las dimensiones deben ser iguales para sumar las matrices"
    | length f1 /= length f2       = Left "Las dimensiones deben ser iguales para sumar las matrices"
    | otherwise                    = Right (zipWith (zipWith (+)) m1 m2)

restarMatrices :: [[Rational]] -> [[Rational]] -> Either String [[Rational]]
restarMatrices [] _ = Left "Las matrices no pueden estar vacias"
restarMatrices _ [] = Left "Las matrices no pueden estar vacias"
restarMatrices m1@(f1:_) m2@(f2:_) = do
    validarMatriz m1
    validarMatriz m2
    restarSiDimensionesIguales m1 m2 f1 f2

restarSiDimensionesIguales :: [[Rational]] -> [[Rational]] -> [Rational] -> [Rational] -> Either String [[Rational]]
restarSiDimensionesIguales m1 m2 f1 f2
    | length m1 /= length m2       = Left "Las dimensiones deben ser iguales para restar las matrices"
    | length f1 /= length f2       = Left "Las dimensiones deben ser iguales para restar las matrices"
    | otherwise                    = Right (zipWith (zipWith (-)) m1 m2)

multiplicarMatrices :: [[Rational]] -> [[Rational]] -> Either String [[Rational]]
multiplicarMatrices [] _ = Left "Las matrices no pueden estar vacias"
multiplicarMatrices _ [] = Left "Las matrices no pueden estar vacias"
multiplicarMatrices m1@(f1:_) m2 = do
    validarMatriz m1
    validarMatriz m2
    multiplicarSiDimensionesCompatibles m1 m2 f1

multiplicarSiDimensionesCompatibles :: [[Rational]] -> [[Rational]] -> [Rational] -> Either String [[Rational]]
multiplicarSiDimensionesCompatibles m1 m2 f1
    | length f1 /= length m2 = Left "El numero de columnas de la primera matriz debe ser igual al numero de filas de la segunda"
    | otherwise              = Right [ [ sum (zipWith (*) fila columna) 
                                       | columna <- transpose m2 
                                       ] 
                                     | fila <- m1 
                                     ]

multiplicarEscalar :: Rational -> [[Rational]] -> Either String [[Rational]]
multiplicarEscalar _ [] = Left "La matriz no puede estar vacia"
multiplicarEscalar escalar mss = do
    validarMatriz mss
    Right (map (map (* escalar)) mss)

transpuesta :: [[Rational]] -> Either String [[Rational]]
transpuesta [] = Left "La matriz no puede estar vacia"
transpuesta mss = do
    validarMatriz mss
    Right (transpose mss)