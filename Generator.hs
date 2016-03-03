-- Module Generator
--   Se encarga de generar el html del scoreboard, en base a la tabla de los equipos.
--   Todas las funciones generan Strings, que son usadas por generate para finalmente
--        imprimir el scoreboard.

module Generator where
import Tipes
import Utils

htmlOut = "Scoreboard.html"
prehtml = "Internos/pre.txt"
poshtml = "Internos/pos.txt"

-- encapsular encierra con un formato un String
encapsular :: String -> String -> String
encapsular = encapsularAttr ""

-- encapsularAttr encierra con un formato un String, agregandole un atributo
encapsularAttr :: String -> String -> String -> String
encapsularAttr a e s = "<" ++ e ++ " " ++ a ++ ">" ++ s ++ "</" ++ e ++ ">" ++ "\n"

printCelda :: String -> String
printCelda = encapsular "td"

printProblem :: Problem -> String
printProblem NotTried = encapsular "td" ""
printProblem (Tried _) = encapsular "td" ""
printProblem (Solved (i,t)) = encapsularAttr ("class=solved") "td" $ show i ++ "<br>" ++ encapsular "small" (show t)

printProblems :: [Problem] -> String
printProblems ps = concat $ map printProblem ps

printName :: Team2 -> String
printName t | zone2 t == Latino = encapsularAttr ("class=latinos") "td" $ name2 t
            | zone2 t == User = encapsularAttr ("class=user") "td" $ name2 t 
            | otherwise = printCelda $ name2 t

printTeam :: Team2 -> String
printTeam t = encapsular "tr" $ (printCelda . show $ position t) ++ (printName t) ++ (printCelda . show $ solved t) ++ (printCelda . show $ penalization t) ++ (printProblems $ problems t)

printTeams :: Teams2 -> String
printTeams ts = concat $ map printTeam ts

printPrevios :: Teams2 -> Int -> String
printPrevios ts i = encapsular "h1" $ "Minuto Actual: " ++ (show i) ++ "<br>"

printHeaders :: Int -> String
printHeaders i = encapsular "tr" $ (encapsular "th" "Pos") ++ (encapsular "th" "Team") ++ (encapsular "th" "Solved") ++ (encapsular "th" "Time") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

printLetters :: Int -> String
printLetters i = encapsular "tr" $ (encapsularAttr "colspan=4" "th" "") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

intentosProb :: Problem -> Int
intentosProb NotTried = 0
intentosProb (Tried i) = i
intentosProb (Solved (i,_)) = i

solvedProb :: Problem -> Bool
solvedProb (Solved _) = True
solvedProb _ = False

totalSolved :: Teams2 -> [Int]
totalSolved ts = foldl (\acc t -> zipWith (+) (map (bool 0 1 . solvedProb) (problems t)) acc) (repeat 0) ts

totalTriesToSolved :: Teams2 -> [Int]
totalTriesToSolved ts = foldl (\acc t -> zipWith (+) (map intentosProb (problems t)) acc) (repeat 0) ts

printSolved :: Teams2 -> String
printSolved ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Equipos que lo resolvieron") ++ ( concat $ map (\n -> printCelda $ show n) $ totalSolved ts)

printTriesToSolve :: Teams2 -> String
printTriesToSolve ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Intentos totales") ++ ( concat $ map (\n -> printCelda $ show n) $ totalTriesToSolved ts)

printCola :: Teams2 -> String
printCola ts = (printLetters $ length $ problems (ts!!0)) ++ printSolved ts ++ (printTriesToSolve ts)

printTable :: Tabla -> String
printTable (i,ts) =  (++) (printPrevios ts i) $ encapsularAttr "id = standings" "table" $ encapsular "tbody" $ (printHeaders $ length $ problems (ts!!0)) ++ printTeams ts ++ printCola ts

-- generate: Dada una lista de teams y un entero que simboliza el tiempo transcurrido, genera un scoreboard.
generate :: Tabla -> IO Html
generate tab = do pre <- readFile prehtml
                  post <- readFile poshtml
                  return $ pre ++ printTable tab ++ post
  
