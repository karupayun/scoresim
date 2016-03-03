-- Module UserTeams
--   Se encarga de trabajar con los equipos que el usuario crea para competir en la simulación.
--   Para eso va guardando la información de los equipos en un archivo interno "myTeams.txt"
--   Ese archivo, de donde solo escribe, lo usará Teams para leer tus teams actuales y así 
--      imprimirlos en el scoreboard generado.

module UserTeams where
import Utils
import Parser (cantProblems)
import Data.Char (ord, toUpper)

type Submit = (Char,Int,Int)

txtMyTeams = "Internos/myTeams.txt"

-- myTeams: Lee la información actual de tus teams
myTeams :: IO [Team] 
myTeams = do r <- readFile txtMyTeams
             let ls = lines r
             let ts = map readTeam ls
             return ts 


-- actualizar: Actualiza la información actual de los tus teams
actualizar :: [Team] -> IO [Team] 
actualizar ts = do writeFile txtMyTeams ""
                   sequence $ map actualizarTeam ts
                   putStrLn $ "actualizar2"
                   return ts

actualizarTeam :: Team -> IO () 
actualizarTeam t = appendFile txtMyTeams (name2 t ++ " - " ++ (concat $ map show $ problems t) ++ " \n")

-- printt: Función auxiliar usada para imprimir un problema en un formato para luego ser leído fácilmente.
--printt :: Problem -> String
--printt NotTried = "NotTried"
--printt (Tried x) = "Tried " ++ show x ++ " "
--printt (Solved(i1,i2)) = "Solved(" ++ show i1 ++ "," ++ show i2 ++ ") "

-- addTeam': Agrega un team a tu conjunto de teams
--addTeam' :: String -> [Team] -> IO [Team] 
--addTeam' s ts = do cp <- cantProblems
--                   let t = Team{
--                             name = s,
--                             solved = 0,
--                            penalization = 0,
--                             problems = replicate cp Nothing,
--                             zone = My,
--                             position = 0}
--                   actualizar $ ts ++ [t]

-- deleteTeam': Borra un team de tus teams.
--deleteTeam' :: Int -> [Team] -> IO [Team] 
--deleteTeam' i ts = actualizar $ (take i ts) ++ (drop (i+1) ts)

-- addSubmit': Agrega información sobre un problema a un equipo específico. 
--               Usa un identificador por team en el orden que fueron creados.
--addSubmit' :: Int -> Submit -> [Team] -> IO [Team]
--addSubmit' id (le,tries,minute) ts = do let t = ts !! id
--                                        let p = problems t
--                                        let nprob = (ord $ toUpper le) - (ord 'A')
--                                       let np = replace (Just (tries, minute)) nprob p -- replace Nothing si minuto = 301
--                                        let nt = Team {
--                                               name = name t,
--                                               solved = solved t,
--                                               penalization = penalization t,
--                                               problems = np,
--                                               zone = zone t,
--                                               position = position t} 
--                                        actualizar $ replace nt id ts
