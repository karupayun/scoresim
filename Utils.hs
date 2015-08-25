-- Modulo Utils
-- Funciones auxiliares y tipos que son usadas a lo largo del simulador.

module Utils where
import Data.List (length, nub)
import Data.Char (isSpace)

-- replace reemplaza un valor en una lista
replace :: a -> Int -> [a] -> [a]
replace item pos lista = do (take pos lista) ++ (item:drop (pos+1) lista)  

-- addVectors suma dos vectores
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) 
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Cantidad de valores únicos en una lista
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Elimina espacios anteriores y posteriores de una lista de Strings
eliminarEspacios :: [String] -> [String] 
eliminarEspacios = map (reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace))

type Problem = Maybe (Int,Int)
data Zone = Latino | My | Other deriving (Eq,Show)
data Team = Team { name :: String
               , solved :: Int
               , penalization :: Int
               , problems :: [Problem]
               , latam :: Zone
               , position :: Int
               } deriving Show


