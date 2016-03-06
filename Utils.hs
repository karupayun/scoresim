-- Modulo Utils
-- Funciones auxiliares que son usadas a lo largo del simulador.

module Utils where
import Tipes
import Data.List (length, nub)
import Data.Char (isSpace)

-- bool : Función auxiliar para trabajar con Bools
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

-- cantProblemsFromTeam :: Calcula la cantidad de problemas dado una lista de Teams
cantProblemsFromTeam :: Teams -> Int
cantProblemsFromTeam [] = 0
cantProblemsFromTeam (x:xs) = (length . subs) x 


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

-- makeSubs inventa las posibles submissions dado la información final de un problema
makeSubs :: Problem -> Submissions
makeSubs NotTried = []
makeSubs (Tried i) = []
makeSubs (Solved (c,m)) = (True,m) : replicate (c-1) (False,m)

-- reRank: Función Auxiliar, dado un Int y un team, le cambia el ranking.
changeZone :: Zone -> Team -> Team
changeZone z t = Team {
             name = name t,
             subs = subs t,
             zone = z,
             user = user t
             }

modifSubs :: Submissionss -> Team -> Team
modifSubs sb t = Team{name = name t,
                      subs = sb,
                      zone = zone t,
                      user = user t} 


