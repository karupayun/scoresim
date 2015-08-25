module Teams where
import Parser (teams, teamsDeLatinoamerica)
import My (myTeams)
import Utils
import Data.List (sortBy)

-- filterTime: Dado un minuto, filtra un problema que todavía no fue resuelto.
filterTime :: Int -> Problem -> Problem
filterTime i p = p >>= \(c,t) -> if (i < t) then Nothing else Just(c,t)

-- probPoint: Dado un minuto, calcula el puntaje actual de ese problema
probPoint :: Int -> Problem -> (Int,Int)
probPoint i p = maybe (0,0) (\(xs,ys) -> (1, ys + 20 * xs - 20)) $ filterTime i p

-- teamPoint: Dado un minuto, calcula el puntaje actual de ese equipo.
teamPoint :: Int -> [Problem] -> (Int,Int)
teamPoint i ps = foldl1 addVectors $ map (probPoint i) ps 

-- better: compara dos teams
better :: Team -> Team -> Ordering
better t1 t2 | solved t1 /= solved t2 = compare (solved t2) (solved t1)
             | penalization t1 /= penalization t2 = compare (penalization t1) (penalization t2)
             | otherwise = compare (name t1) (name t2)

-- reRank: Función Auxiliar, dado un Int y un team, le cambia el ranking.
reRank :: (Int,Team) -> Team
reRank (i,t) = Team {
             name = name t,
             solved = solved t,
             penalization = penalization t,
             problems = problems t,
             latam = latam t,
             position = i}

-- TableTime: Dado un minuto, devuelve la información de los equipos
--                del simulacro al minuto i.
tableTime :: Int -> IO [Team]
tableTime i = do ts <- teams 
                 lat <- teamsDeLatinoamerica  
                 my <- myTeams           
                 let p = foldr (\t acc -> Team{
                             name = name t,
                             solved = fst (teamPoint i $ problems t),
                             penalization = snd (teamPoint i $ problems t),
                             problems = map (filterTime i) $ problems t,
                             latam = if ((name t) `elem` lat) then Latino else if ((name t) `elem` map name my) then My else Other,
                             position = position t}:acc) [] (ts++my)
                 return $ map reRank $ zip [1..] $ sortBy better p
                 
                
