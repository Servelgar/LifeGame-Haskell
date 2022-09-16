
-- Encontrar una asignación a n reinas en un tablero n x n de modo que no se ataquen.

-- El problema tiene dos versiones. La primera es encontrar una solución válida dado un 
-- valor n. La otra versión consiste en encontrar todas las soluciones posibles para el valor n

-------------

-- Found an assignation to n queens in a square n x n so they can't interact with each other

-- This problem has two versions. The first one is to found the correct solution given n value. The other
-- version needs to found all possible solutions to n value.



---------------------------------------------------------------------------------------------------


import Data.Array
import Data.Bool
import Data.List


type Square = Array (Int,Int) Bool


n = 8


s1 :: Square
s1 = listArray ((1,1), (n,n)) (replicate (n*n) False)

s2 = listArray ((1,1), (n,n)) (True : replicate (n*n - 1) False)

-- Restrictions 


agrupar :: [a] -> [[a]]
agrupar [] = []
agrupar xs = take n xs : agrupar (drop n xs) 


rHorizontal :: Square -> Int -> Bool
rHorizontal ss j = sum [if ss!(x,j) then 1 else 0 | x <- [1..n]] == 1

rVertical :: Square -> Int -> Bool
rVertical ss i = sum [if ss!(i,y) then 1 else 0 | y <- [1..n]] == 1

rDiagonalPos :: Square -> Int -> Int -> Bool
rDiagonalPos ss i j = sum [if ss!(x,y) then 1 else 0 | x <- [1..n], y <- [1..n], x /= y, abs(x-y) /= ] == 1

rDiagonalNeg :: Square -> Int -> Int -> Bool
rDiagonalNeg ss i j = sum [if ss!(x,y) then 1 else 0 | k <- [(1-n)..(n-1)], x<-[1..n], y <-[1..n], x-y == k] == 1

aplicarReglas :: Square -> Int -> Int -> Bool
aplicarReglas ss i j = rHorizontal ss j && rVertical ss i && rDiagonalPos ss i j && rDiagonalNeg ss i j

colocarReina :: Square -> Int -> Int -> Square
colocarReina ss i j = listArray (bounds ss) ([(i == x && j == y) || (ss!(x,y)) | x <- [1..n], y <- [1..n]])

compruebaSol :: Square -> Bool
compruebaSol ss = (sum [if x then 1 else 0 | x <- elems ss]) == n


huecosDispo ss i j = [(x,y) | x <- [1..n], y<-[1..n], not $ aplicarReglas ss x y ]


solReinas :: Int -> Square -> Int -> Int -> (Int,Square)
solReinas k ss i j
    | k == n = (k,ss)
    | i == n && j == n = (k,ss)
    | i == n = solReinas k ss 1 (j+1)
    | otherwise = if aplicarReglas nss i j then
         solReinas (k+1) nss (i+1) j else solReinas k ss (i+1) j
    where
        nss = colocarReina ss i j


generaArbol ss = [solReinas 1 (nss ss i 1) i 1 | i <- [1..n]]
    where 
        nss  = colocarReina 

res xss = head [y | (x,y) <- xss, x == n]



main = do 
    let (sol, ss) = solReinas 1 s2 1 1
    print sol
    putStrLn ""
    putStrLn ""
    putStrLn "" 
    print $ agrupar $ elems ss