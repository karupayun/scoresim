-- Module Generator
-- Se encarga de generar el html del scoreboard, en base a la tabla actual de los equipos. 
-- Todas las funciones auxiliares generan Strings, que son usadas por generate para finalmente imprimir el scoreboard completo.
-- Por razones de simplicidad y de generar un scoreboard igual al que proveen en el mundial de programación, se utilizan dos archivos que tienen las directivas usadas para lograr el mismo formato.

module HtmlContestPrinter (generate) where
import Tipes
import Auxiliar (bool)

-- Archivos auxiliares. Usados para dar un formato similar al formato Kattis usado en el mundial. Simplemente son copias de la parte previa y posterior de una tabla.
prehtml = "Internos/pre.txt"
poshtml = "Internos/pos.txt"

-- encapsular encierra con un formato un String
encapsular :: String -> String -> String
encapsular = encapsularAttr ""

-- encapsularAttr :: Función Auxiliar. encierra con un formato un String, agregandole un atributo
encapsularAttr :: String -> String -> String -> String
encapsularAttr a e s = "<" ++ e ++ " " ++ a ++ ">" ++ s ++ "</" ++ e ++ ">" ++ "\n"

-- printCelda :: Imprime una celda de la tabla
printCelda :: String -> String
printCelda = encapsular "td"

printProblem :: Problem -> String
printProblem NotTried = encapsular "td" ""
printProblem (Tried _) = encapsular "td" ""
printProblem (Solved (i,t)) = encapsularAttr ("class=solved") "td" $ show i ++ "<br>" ++ encapsular "small" (show t)

printProblems :: [Problem] -> String
printProblems ps = concat $ map printProblem ps

printName :: TeamLive -> String
printName t | _zone2 t == Latino = encapsularAttr ("class=latinos") "td" $ _name2 t
            | _zone2 t == User = encapsularAttr ("class=user") "td" $ _name2 t 
            | otherwise = printCelda $ _name2 t

printTeam :: TeamLive -> String
printTeam t = encapsular "tr" $ (printCelda . show $ _position t) ++ (printName t) ++ (printCelda . show $ _solved t) ++ (printCelda . show $ _penalization t) ++ (printProblems $ _problems t)

printTeams :: TeamsLive -> String
printTeams ts = concat $ map printTeam ts

printPrevios :: TeamsLive -> Int -> String
printPrevios ts i = encapsular "h1" $ "Minuto Actual: " ++ (show i) ++ "<br>"

printHeaders :: Int -> String
printHeaders i = encapsular "tr" $ (encapsular "th" "Pos") ++ (encapsular "th" "Team") ++ (encapsular "th" "Solved") ++ (encapsular "th" "Time") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

printLetters :: Int -> String
printLetters i = encapsular "tr" $ (encapsularAttr "colspan=4" "th" "") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

-- Funciones Auxiliares usadas para calcular la cantidad de intentos y problemas resueltos.
intentosProb :: Problem -> Int
intentosProb NotTried = 0
intentosProb (Tried i) = i
intentosProb (Solved (i,_)) = i

solvedProb :: Problem -> Bool
solvedProb (Solved _) = True
solvedProb _ = False


totalSolved :: TeamsLive -> [Int]
totalSolved ts = foldl (\acc t -> zipWith (+) (map (bool 0 1 . solvedProb) (_problems t)) acc) (repeat 0) ts

totalTriesToSolved :: TeamsLive -> [Int]
totalTriesToSolved ts = foldl (\acc t -> zipWith (+) (map intentosProb (_problems t)) acc) (repeat 0) ts

printSolved :: TeamsLive -> String
printSolved ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Equipos que lo resolvieron") ++ ( concat $ map (\n -> printCelda $ show n) $ totalSolved ts)

printTriesToSolve :: TeamsLive -> String
printTriesToSolve ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Intentos totales") ++ ( concat $ map (\n -> printCelda $ show n) $ totalTriesToSolved ts)

printCola :: TeamsLive -> String
printCola ts = (printLetters $ length $ _problems (ts!!0)) ++ printSolved ts ++ (printTriesToSolve ts)

printTable :: Tabla -> String
printTable (i,ts) =  (++) (printPrevios ts i) $ encapsularAttr "id = standings" "table" $ encapsular "tbody" $ (printHeaders $ length $ _problems (ts!!0)) ++ printTeams ts ++ printCola ts

-- generate :: Dada una lista de teams y un entero que simboliza el tiempo transcurrido, genera un scoreboard, ayudándose con los archivos 
--     pre y post, para darle el formato que es provisto en el mundial de programación.
generate :: Tabla -> IO Html
generate tab = do pre <- readFile prehtml
                  post <- readFile poshtml
                  return $ pre ++ printTable tab ++ post
  
