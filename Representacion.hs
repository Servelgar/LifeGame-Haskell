{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import qualified ModuloJuego as M (
    red,
    agrupar,
    agruparGeneracion,
    agruparGeneracionSinPunto,
    semillaSistema,
    calculaGeneracion)

import qualified ReglasJuego as R(
  Celula(VIVA, MUERTA),
  Punto,
    Red,
    alto,
    ancho,
    getVecinos,
    getVecinosCoor,
    calculaVecinosVivos,
    destinoCelulaMuerta,
    destinoCelulaViva)

import System.Random
import System.IO
import GHC.Exts.Heap (GenClosure(FloatClosure))
import Distribution.Simple (Extension(EnableExtension))
import Data.List
import Data.Ratio
import Control.Monad
import Data.Vector.Internal.Check (doChecks) 
import Data.Matrix as Ma
import Data.Char (isDigit)
import qualified Language.Haskell.TH as R
import qualified Language.Haskell.Exts as R
import ModuloJuego (agruparGeneracion)

{-
------------------ FUNCIONES OPCIONALES - OBSOLETAS A PARTIR POR CODEWORLD -------------

leeYContinua :: Matrix (R.Punto, R.Celula) -> Int -> [Char] ->  IO ()
leeYContinua gen i acc = do
    c <- getChar
    case c of
      's' -> return ()
      '\n' -> do imprimeGeneraciones gen i
      _ -> leeYContinua gen i (c:acc)



imprimeGeneraciones :: Matrix (R.Punto, R.Celula) -> Int -> IO ()
imprimeGeneraciones gen i = do
     let listaGen = M.calculaGeneracion gen 
     let genSiguiente = M.agruparGeneracion R.alto listaGen
     let genSinPunto = M.agruparGeneracionSinPunto R.alto listaGen
     let linea = show i ++ "º generacion: "
     putStrLn linea
     print genSinPunto
     drawingOf $ minimap genSinPunto
     putStrLn "Pulsa 's' para salir o cualquier otra letra para continuar"
     leeYContinua genSiguiente (i+1) "" 

-}



minimap :: Matrix R.Celula -> Picture
minimap testMap = 
  pictures [cell i j | i <- [1..R.alto], j <- [1..R.alto]]
 where
  cell i j = translated (fromIntegral i-10) (fromIntegral j-10) 
             $ colored (minimapColor (testMap Ma.! (i,j)))
             $ solidRectangle 1 1


minimapColor :: R.Celula -> Color
minimapColor R.MUERTA  = grey
minimapColor R.VIVA  = green


siguienteEstado :: Event -> Matrix R.Celula -> Matrix R.Celula
siguienteEstado evento m =
  case evento of 
    KeyPress "Enter" -> nuevaMatriz
    _ -> m
    where
      nuevaMatriz =  
        M.agruparGeneracionSinPunto R.alto $ M.calculaGeneracion
         $ agruparGeneracion R.alto $ concat $ toLists m

{- MAIN -}

main :: IO ()
main =  do
  numR <-  replicateM (R.alto*R.alto) (randomRIO (0, 1 :: Double))
  let semilla = M.semillaSistema R.alto numR
  let semillaSinPunto = M.agruparGeneracionSinPunto R.alto  [c | ((_,_),c) <- toList semilla]
--  let listaGen = M.calculaGeneracion semilla
 -- let gen = M.agruparGeneracion R.alto listaGen 
--  let genSinPunto = M.agruparGeneracionSinPunto R.alto listaGen
 -- let testMap = M.agrupar R.alto listaGen
  putStrLn  "La semilla inicial, generada aleatoriamente, es:"
  --print semillaSinPunto
--  putStrLn  "2º generación: "
--  print genSinPunto
  activityOf semillaSinPunto siguienteEstado minimap  
--  putStrLn "Pulsa 's' para salir o cualquier otra letra para continuar."
--  leeYContinua gen 3 ""


-- las líneas comentadas sirven para la representación en consola el juego -- obsoleto con codeworld