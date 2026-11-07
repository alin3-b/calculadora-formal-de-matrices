{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson
import Data.Ratio (Rational, numerator, denominator, (%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Cors
    ( cors
    , CorsResourcePolicy(..)
    , simpleCorsResourcePolicy
    )

import Calculadora

data Solicitud = Solicitud
    { operacion :: Text
    , matrizA :: Maybe [[Text]]
    , matrizB :: Maybe [[Text]]
    , dimension :: Maybe Int
    , fila :: Maybe Int
    , columna :: Maybe Int
    , escalar :: Maybe Text
    }

instance FromJSON Solicitud where
    parseJSON = withObject "Solicitud" $ \obj ->
        Solicitud
            <$> obj .: "operacion"
            <*> obj .:? "matrizA"
            <*> obj .:? "matrizB"
            <*> obj .:? "dimension"
            <*> obj .:? "fila"
            <*> obj .:? "columna"
            <*> obj .:? "escalar"

parseEntero :: Text -> Either String Integer
parseEntero texto =
    case reads (T.unpack texto) of
        [(numero, "")] -> Right numero
        _ -> Left ("Número entero inválido: " ++ T.unpack texto)

parseRacional :: Text -> Either String Rational
parseRacional texto =
    let limpio = T.strip texto
    in case T.splitOn "/" limpio of
        [numero] -> parseDecimal numero

        [n, d] -> do
            numeradorN <- parseEntero n
            denominadorN <- parseEntero d

            if denominadorN == 0
                then Left "El denominador no puede ser cero"
                else Right (numeradorN % denominadorN)

        _ -> Left ("Fracción inválida: " ++ T.unpack limpio)

parseDecimal :: Text -> Either String Rational
parseDecimal texto =
    let (signo, cuerpo) =
            if T.isPrefixOf "-" texto
                then (-1, T.drop 1 texto)
                else if T.isPrefixOf "+" texto
                    then (1, T.drop 1 texto)
                    else (1, texto)

        partes = T.splitOn "." cuerpo

    in case partes of
        [entero] -> do
            numero <- parseEntero entero
            Right (fromIntegral signo * numero % 1)

        [entero, decimal]
            | not (T.null entero)
            && not (T.null decimal)
            && T.all isDigit entero
            && T.all isDigit decimal -> do

                let escala = 10 ^ T.length decimal
                numero <- parseEntero (entero <> decimal)

                Right (fromIntegral signo * numero % escala)

        _ -> Left ("Número decimal inválido: " ++ T.unpack texto)

convertirMatriz :: [[Text]] -> Either String [[Rational]]
convertirMatriz = traverse (traverse parseRacional)

mostrarRacional :: Rational -> String
mostrarRacional numero
    | denominator numero == 1 =
        show (numerator numero)
    | otherwise =
        show (numerator numero) ++ "/" ++ show (denominator numero)

calcular :: Solicitud -> Either String Value
calcular solicitud =
    case operacion solicitud of

        "suma" -> do
            a <- obtenerMatrizA solicitud
            b <- obtenerMatrizB solicitud
            resultado <- sumarMatrices a b
            Right (matrizJSON resultado)

        "resta" -> do
            a <- obtenerMatrizA solicitud
            b <- obtenerMatrizB solicitud
            resultado <- restarMatrices a b
            Right (matrizJSON resultado)

        "determinante" -> do
            a <- obtenerMatrizA solicitud
            resultado <- determinante a
            Right (numeroJSON resultado)

        "traza" -> do
            a <- obtenerMatrizA solicitud
            resultado <- traza a
            Right (numeroJSON resultado)

        "transpuesta" -> do
            a <- obtenerMatrizA solicitud
            resultado <- transpuesta a
            Right (matrizJSON resultado)

        "identidad" -> do
            n <- obtenerDimension solicitud
            resultado <- matrizIdentidad n
            Right (matrizJSON resultado)

        "inversa" -> do
            a <- obtenerMatrizA solicitud
            resultado <- inversa a
            Right (matrizJSON resultado)

        "cofactores" -> do
            a <- obtenerMatrizA solicitud
            resultado <- matrizCofactores a
            Right (matrizJSON resultado)

        "adjunta" -> do
            a <- obtenerMatrizA solicitud
            resultado <- matrizAdjunta a
            Right (matrizJSON resultado)

        "cofactor" -> do
            a <- obtenerMatrizA solicitud
            i <- obtenerFila solicitud
            j <- obtenerColumna solicitud
            resultado <- cofactor i j a
            Right (numeroJSON resultado)

        "invertible" -> do
            a <- obtenerMatrizA solicitud
            resultado <- verificarInvertible a
            Right (toJSON resultado)

        "verificarInversa" -> do
            a <- obtenerMatrizA solicitud
            resultado <- verificarInversa a
            Right (toJSON resultado)

        "simetrica" -> do
            a <- obtenerMatrizA solicitud
            resultado <- esSimetrica a
            Right (toJSON resultado)

        "multiplicacion" -> do
            a <- obtenerMatrizA solicitud
            b <- obtenerMatrizB solicitud
            resultado <- multiplicarMatrices a b
            Right (matrizJSON resultado)

        "multiplicacionEscalar" -> do
            a <- obtenerMatrizA solicitud
            numero <- obtenerEscalar solicitud
            resultado <- multiplicarEscalar numero a
            Right (matrizJSON resultado)

        "submatriz" -> do
            a <- obtenerMatrizA solicitud
            i <- obtenerFila solicitud
            j <- obtenerColumna solicitud
            resultado <- submatriz i j a
            Right (matrizJSON resultado)

        "dimensionMatriz" -> do
            a <- obtenerMatrizA solicitud
            n <- matrizDim a
            Right (toJSON n)

        otra ->
            Left ("Operación todavía no implementada: " ++ T.unpack otra)

obtenerMatrizB :: Solicitud -> Either String [[Rational]]
obtenerMatrizB solicitud =
    case matrizB solicitud of
        Nothing -> Left "Esta operación requiere una matriz B"
        Just valores -> convertirMatriz valores

obtenerMatrizA :: Solicitud -> Either String [[Rational]]
obtenerMatrizA solicitud =
    case matrizA solicitud of
        Nothing -> Left "Esta operacion requiere una matriz A"
        Just valores -> convertirMatriz valores

obtenerDimension :: Solicitud -> Either String Int
obtenerDimension solicitud =
    case dimension solicitud of
        Nothing -> Left "Esta operacion requiere especificar la dimension"
        Just n -> Right n

obtenerFila :: Solicitud -> Either String Int
obtenerFila solicitud =
    case fila solicitud of
        Nothing -> Left "Debes proporcionar la fila"
        Just n -> Right n


obtenerColumna :: Solicitud -> Either String Int
obtenerColumna solicitud =
    case columna solicitud of
        Nothing -> Left "Debes proporcionar la columna"
        Just n -> Right n

obtenerEscalar :: Solicitud -> Either String Rational
obtenerEscalar solicitud =
    case escalar solicitud of
        Nothing -> Left "Debes proporcionar el escalar"
        Just valor -> parseRacional valor

matrizJSON :: [[Rational]] -> Value
matrizJSON mss = toJSON (map (map mostrarRacional) mss)

numeroJSON :: Rational -> Value
numeroJSON num = toJSON (mostrarRacional num)

main :: IO ()
main = scotty 3000 $ do

    middleware $ cors (const $ Just politicaCors)

    get "/" $ do
        text "Servidor de la Calculadora de Matrices funcionando"

    post "/api/calcular" $ do
        solicitud <- jsonData

        case calcular solicitud of
            Left mensaje -> do
                status status400
                json $ object
                    [ "error" .= mensaje
                    ]

            Right resultado -> do
                json $ object
                    [ "resultado" .= resultado
                    ]

    where
        politicaCors = simpleCorsResourcePolicy
            { corsOrigins = Just
                ( [ "http://127.0.0.1:5500"
                  , "http://localhost:5500"
                  ]
                , False
                )
            , corsMethods = ["GET", "POST", "OPTIONS"]
            , corsRequestHeaders = ["Content-Type"]
            }