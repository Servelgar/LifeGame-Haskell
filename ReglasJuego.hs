module ReglasJuego
(   
    Punto,
    Celula(..),
    Red,
    alto,
    ancho,
    getVecinos,
    getVecinosCoor,
    calculaVecinosVivos,
    destinoCelulaMuerta,
    destinoCelulaViva
) where 

alto = 10
ancho = alto 

type Punto = (Int, Int)

data Celula = VIVA | MUERTA deriving (Eq, Ord, Read, Show)

type Red = [Punto]

-- Cada célula tendrá 8 vecinos, pero para poder calcular las generaciones solo necesitamos quienes
-- están vivos (los vecinos).


getVecinosCoor :: Punto -> [Punto]
getVecinosCoor (x,y) = 
    [(x+i, y+j) | i <- [-1,0,1], j <- [-1,0,1], (i,j) /= (0,0), x+i <= ancho-1, y+j <= alto-1, x+i >= 0, y+j >= 0 ]


getVecinos :: [(Punto, Celula)] -> (Int, Int) -> [Celula]
getVecinos matriz (x,y) = [c | (coor,c) <- matriz, coor `elem` vecinosCoor]
    where 
        vecinosCoor = getVecinosCoor (x,y)


destinoCelulaViva :: (Ord a, Num a) => a -> Celula
destinoCelulaViva vecinos
    | vecinos < 2 = MUERTA
    | vecinos == 2 || vecinos == 3 = VIVA
    | otherwise = MUERTA


destinoCelulaMuerta :: (Eq a, Num a) => a -> Celula
destinoCelulaMuerta vecinos
    | vecinos /= 3 = MUERTA
    | otherwise = VIVA


calculaVecinosVivos :: Num p => [Celula] -> p
calculaVecinosVivos [] = 0
calculaVecinosVivos cs = sum [1 | c <- cs, c == VIVA]