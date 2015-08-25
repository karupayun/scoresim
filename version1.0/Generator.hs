-- Module Generator
--   Se encarga de generar el html del scoreboard, en base a la tabla de los equipos.
--   Todas las funciones generan Strings, que son usadas por generate para finalmente
--        imprimir el scoreboard.

module Generator where
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
printProblem Nothing = encapsular "td" ""
printProblem (Just (i,t)) = encapsularAttr ("class=solved") "td" $ show i ++ "<br>" ++ encapsular "small" (show t)

printProblems :: [Problem] -> String
printProblems ps = concat $ map printProblem ps

printName :: Team -> String
printName t | latam t == Latino = encapsularAttr ("class=latinos") "td" $ name t
            | latam t == My = encapsularAttr ("class=pending") "td" $ name t
            | otherwise = printCelda $ name t

printTeam :: Team -> String
printTeam t = encapsular "tr" $ (printCelda . show $ position t) ++ (printName t) ++ (printCelda . show $ solved t) ++ (printCelda . show $ penalization t) ++ (printProblems $ problems t)

printTeams :: [Team] -> String
printTeams ts = concat $ map printTeam ts

printPrevios :: [Team] -> Int -> String
printPrevios ts i = encapsular "h1" $ "Minuto Actual: " ++ (show i) ++ "<br>"

printHeaders :: Int -> String
printHeaders i = encapsular "tr" $ (encapsular "th" "Pos") ++ (encapsular "th" "Team") ++ (encapsular "th" "Solved") ++ (encapsular "th" "Time") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

printLetters :: Int -> String
printLetters i = encapsular "tr" $ (encapsularAttr "colspan=4" "th" "") ++ (foldr (\c acc -> (encapsular "th" [c])++ acc) "" (take i ['A'..]))

totalSolved :: [Team] -> [Int]
totalSolved ts = foldl (\acc t -> zipWith (+) (map (maybe 0 (\_ -> 1)) (problems t)) acc) (repeat 0) ts

totalTriesToSolved :: [Team] -> [Int]
totalTriesToSolved ts = foldl (\acc t -> zipWith (+) (map (maybe 0 fst) (problems t)) acc) (repeat 0) ts

printSolved :: [Team] -> String
printSolved ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Equipos que lo resolvieron") ++ ( concat $ map (\n -> printCelda $ show n) $ totalSolved ts)

printTriesToSolve :: [Team] -> String
printTriesToSolve ts = encapsular "tr" $ (encapsularAttr "colspan=4" "td" "Cantidad de Intentos totales") ++ ( concat $ map (\n -> printCelda $ show n) $ totalTriesToSolved ts)

printCola :: [Team] -> String
printCola ts = (printLetters $ length $ problems (ts!!0)) ++ printSolved ts ++ (printTriesToSolve ts)

printTable :: [Team] -> Int -> String
printTable ts i =  (++) (printPrevios ts i) $ encapsularAttr "id = standings" "table" $ encapsular "tbody" $ (printHeaders $ length $ problems (ts!!0)) ++ printTeams ts ++ printCola ts

-- generate: Dada una lista de teams y un entero que simboliza el tiempo transcurrido, genera un scoreboard.
generate :: [Team] -> Int -> IO ()
generate ts i = do pre <- readFile prehtml
                   post <- readFile poshtml
                   writeFile htmlOut pre
                   appendFile htmlOut $ printTable ts i
                   appendFile htmlOut post
 
