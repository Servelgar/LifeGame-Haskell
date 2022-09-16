module ModuloJuego(
    red,
    agrupar,
    agruparGeneracion,
    agruparGeneracionSinPunto,
    semillaSistema,
    calculaGeneracion,
) where


import System.Random
import System.IO
import GHC.Exts.Heap (GenClosure(FloatClosure))
import Distribution.Simple (Extension(EnableExtension))
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




-- Red de puntos según la altura y anchura

red :: R.Red
red  = [(x,y) | x <- [0..R.ancho-1], y <- [0..R.ancho-1]]


-- Función auxiliar para agrupar listas en listas de listas de n elementos
agrupar :: Int -> [a] -> [[a]]
agrupar _ [] = []
agrupar n l = take n l : agrupar n (drop n l)

agrupar' :: Foldable t => Int -> p -> t [a] -> [[a]]
agrupar' n l = foldl (\y x -> take n x : drop n y) [[]]

-- Primera matriz del sistema
semillaSistema :: (Ord a, Fractional a) => Int -> [a] -> Matrix (R.Punto, R.Celula)
semillaSistema tam numR = fromLists $ agrupar tam $ zip red $ replicar (tam*tam) numR
            where
                replicar t [] = [] 
                replicar t (r:rs)
                    | r > 0.5 = R.VIVA:replicar (t-1) rs
                    | otherwise = R.MUERTA:replicar (t-1) rs


-- Calcula la siguiente generación 

calculaGeneracion :: Matrix (R.Punto, R.Celula) -> [R.Celula]
calculaGeneracion m = aux $ concat $ toLists m
    where
        aux red = [if c == R.VIVA then 
            R.destinoCelulaViva $ R.calculaVecinosVivos $ R.getVecinos red coor else 
                R.destinoCelulaMuerta $ R.calculaVecinosVivos $ R.getVecinos red coor |(coor,c) <- red]


agruparGeneracion :: Int -> [b] -> Matrix (R.Punto, b)
agruparGeneracion tam lg = fromLists $ agrupar tam $ zip red lg 

agruparGeneracionSinPunto :: Int -> [a] -> Matrix a
agruparGeneracionSinPunto tam lg = fromLists $ agrupar tam lg


