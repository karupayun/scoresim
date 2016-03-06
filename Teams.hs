-- Modulo Teams
--   El módulo lógico que trabaja con los equipos de un simulacro.
--     Provee una función que dada la información de los equipos,
--     calcula la tabla de los equipos ordenadas al minuto.


module Teams where
import Utils
import Tipes
import Data.List (sortBy)

-- countWA :: Dado un minuto, cuenta la cantidad de submission no aceptadas.
countWA :: Int -> Submissions -> Int
countWA m s = length $ filter (\(b,i) -> b == False && i <= m) s

-- subsProb: Dado un minuto, calcula la situación de un problema dadas las submissions.
subsProb :: Int -> Submissions -> Problem
subsProb m s | m < firstSB = NotTried
               | m < firstAC = Tried $ countWA m s
               | otherwise = Solved (1+countWA firstAC s, firstAC)              
                where firstAC = foldl (\ac (b,i) -> if b then min ac i else ac) 301 s 
                      firstSB = foldl (\ac (_,i) -> min ac i ) 301 s 

-- probPoint: Calcula el puntaje real de un problema
probPoint :: Problem -> (Int,Int)
probPoint NotTried = (0,0) 
probPoint (Tried _) = (0,0) 
probPoint (Solved (i,t)) = (1,t+20*i-20) 

-- teamPoint: Dado un minuto, calcula el puntaje actual de este equipo.
teamPoint :: Int -> Submissionss -> (Int,Int)
teamPoint i ss = foldl1 addVectors $ map (probPoint . subsProb i) ss 

-- reRank: Función Auxiliar, dado un Int y un team, le cambia el ranking.
reRank :: (Int,TeamLive) -> TeamLive
reRank (i,t) = TeamLive {
             name2 = name2 t,
             solved = solved t,
             penalization = penalization t,
             problems = problems t,
             zone2 = zone2 t,
             user2 = user2 t,
             position = i
            }

-- better: compara dos teams
better :: TeamLive -> TeamLive -> Ordering
better t1 t2 | solved t1 /= solved t2 = compare (solved t2) (solved t1)
             | penalization t1 /= penalization t2 = compare (penalization t1) (penalization t2)
             | otherwise = compare (name2 t1) (name2 t2)

-- crearTabla: Dado un minuto del simulacro y los equipos, lo transforma a forma mostrable y lo ordena según el formato usado para las competencias.
--                   Este Contest está listo para ser mostrado.
crearTabla :: Int -> Teams -> Tabla
crearTabla i ts = let t = foldr (\t acc -> TeamLive {
                                     name2 = name t,
                                     solved = fst (teamPoint i $ subs t),
                                     penalization = snd (teamPoint i $ subs t),
                                     problems = map (subsProb i) $ subs t,
                                     zone2 = zone t,
                                     position = 0,
                                     user2 = user t
                                    }:acc) [] ts                             
                     in (i, map reRank $ zip [1..] $ sortBy better t)


                 
                
