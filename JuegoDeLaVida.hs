
{-

El universo del Juego de la Via es una infinita, bidimensional red ortogonal de células cuadradas, cada
una de ellas tiene dos posibles estados: viva o muerta. Cada célula interectua con sus ocho vecinos
adyacentes a ella. Cada paso en el tiempo, se siguen estas reglas:

1. Cada célula viva con menos de dos vecinos vivos muere.
2. Cada célula viva con dos o tres vecinos vivos vive a la siguiente generación.
3. Cada célula con más de tres vecinos vivos muere
4. Cada célula muerta que tiene exactamente tres vecinos vivos se convierte en una célula viva.

El patrón inicial constituye la semilla del sistema. La primera generación es creada aplicando las reglas 
de arriba simultáneamente a cada célula de la semilla, viva o muerta.


-}

import System.Random
import System.IO
import GHC.Exts.Heap (GenClosure(FloatClosure))
import Distribution.Simple (Extension(EnableExtension))
--import System.Win32 (kEYEVENTF_EXTENDEDKEY)
import Data.List
import Data.Ratio
import Control.Monad.Trans.State
import Control.Monad
import Data.Vector.Internal.Check (doChecks)
import Data.Matrix
import Data.Char (isDigit)
import qualified ReglasJuego as R(Celula(VIVA, MUERTA),
 Punto,
    Red,
    alto,
    ancho,
    getVecinos,
    getVecinosCoor,
    calculaVecinosVivos,
    destinoCelulaMuerta,
    destinoCelulaViva)


-- Alto y ancho de la red


-- Importa poco usar alto o ancho, siendo dimensiones cuadradas



red :: R.Red
red  = [(x,y) | x <- [0..R.ancho-1], y <- [0..R.ancho-1]]



agrupar :: Int -> [a] -> [[a]]
agrupar _ [] = []
agrupar n l = take n l : agrupar n (drop n l)

semillaSistema :: (Ord a, Fractional a) => Int -> [a] -> Matrix (R.Punto, R.Celula)
semillaSistema tam numR = fromLists $ agrupar tam $ zip red $ replicar (tam*tam) numR
            where
                replicar t [] = [] 
                replicar t (r:rs)
                    | r > 0.5 = R.VIVA:replicar (t-1) rs
                    | otherwise = R.MUERTA:replicar (t-1) rs

calculaGeneracion :: Matrix (R.Punto, R.Celula) -> [R.Celula]
calculaGeneracion m = aux $ concat $ toLists m
    where
        aux red = [if c == R.VIVA then 
            R.destinoCelulaViva $ R.calculaVecinosVivos $ R.getVecinos red coor else 
                R.destinoCelulaMuerta $ R.calculaVecinosVivos $ R.getVecinos red coor |(coor,c) <- red]
        -- aux [] = [] 
        -- aux red@((coor,c):matriz) 
            --  | c == Viva = destinoCelulaViva (vecinos) : aux matriz
            --  | otherwise = destinoCelulaMuerta (vecinos) : aux matriz
            --     where
            --         vecinos = calculaVecinosVivos (getVecinos red coor)
                    
agruparGeneracion tam lg = fromLists $ agrupar tam $ zip red lg 


-- Hasta aquí se puede ir calculando generaciones. Por lo que quedaría:
    -- Función por patrones, case of, contar celulas vivas, aleatoriedad semillaSistema
    -- 0)  CASE OF, 
    -- 1) Hacer el proceso "automatizado"
    -- 2) Representarlo por consola o pantalla
    -- 2a) Se me ocurre si es posible dar la imagen de la primera generación y con un botón ir avanzando de generaciones
    -- 2b) La segunda alternativa, aparentemente más dificil, es una animación de las generaciones en vivo


leeYContinua :: Matrix (R.Punto, R.Celula) -> Int -> [Char] ->  IO ()
leeYContinua gen i acc = do
    c <- getChar
    case c of
      's' -> return ()
      '\n' -> do imprimeGeneraciones gen i
      _ -> leeYContinua gen i (c:acc)



imprimeGeneraciones :: Matrix (R.Punto, R.Celula) -> Int -> IO ()
imprimeGeneraciones gen i = do
     let listaGen = calculaGeneracion gen 
     let genSiguiente = agruparGeneracion R.alto listaGen
     let linea = show i ++ "º generacion: "
     putStrLn linea
     print genSiguiente
     putStrLn "Pulsa 's' para salir o cualquier otra letra para continuar"
     leeYContinua genSiguiente (i+1) "" 

------------------------------------------------x------------------------------------------------------------------------------
-----------------------------------------------------------o-------------------------------------------------------------------
------------------------------------------------------------------------x------------------------------------------------------

-- Supongamos una matriz de altoxalto de ejemplo
main :: IO ()
main = do
     numR <-  replicateM (R.alto*R.alto) (randomRIO (0, 1 :: Double))
     let semilla = semillaSistema R.alto numR
     let listaGen = calculaGeneracion semilla
     let gen = agruparGeneracion R.alto listaGen 
     putStrLn  "La semilla inicial, generada aleatoriamente, es:"
     print semilla
     putStrLn  "2º generación: "
     print gen
     putStrLn "Pulsa 's' para salir o cualquier otra letra para continuar."
     leeYContinua gen 3 ""
    











{-
semillaSistema :: (Num a, Ord a) => a -> [(Punto, Celula)]
semillaSistema tam = zip red (replicar (tam*tam) (generaNumR (tam*tam)))
    where
        replicar t [] = [] 
        replicar t (r:rs)
            | r > 0.5 = Viva:replicar (t-1) rs
            | otherwise = Muerta:replicar (t-1) rs
-}


{-
semillaSistema :: (Num a, Ord a) => a -> [(Punto, Celula)]
semillaSistema tam = zip red ( replicar (tam*tam) Muerta)
    where 
        replicar 0 _ = []
        replicar _ Viva = []
        replicar tam Muerta
           | tam > 0 = Muerta : replicar (tam-1) Muerta 
           | otherwise = replicar (tam-1) Muerta
-}

-- calculaGeneracion :: [(Punto, Celula)] -> [Celula]
-- calculaGeneracion [] = []
-- calculaGeneracion ((coor,c):red) 
--     | c == Viva = destinoCelulaViva vecinos : calculaGeneracion red
--     | otherwise = destinoCelulaMuerta vecinos : calculaGeneracion red
--         where
--             vecinos = calculaVecinosVivos (getVecinos red coor)