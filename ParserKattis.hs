-- Modulo ParserKattis
--   Parsea un scoreboard de Kattis en formato html, y transforma la información
--     en un formato deseable, para luego ser trabajada. 
--   Parser dado de ejemplo.

module ParserKattis where
import Utils
import Tipes
import Text.XML.HXT.Core
import Text.HandsomeSoup (css)
import Data.List (findIndex)
import Data.Maybe (fromJust)

type Celda = [String]
type Fila = [Celda]


--Constantes del Parser, sirven para hacer el código más reusable ante 
--  la diversidad de formatos distintos de scoreboards
htmlIn = "Internos/in.html"
standings = "standings"
tr = "tr"
title = "title"
latTeams = "Latin America"
latamChampion = "Super region leader, Latin America"
latinos = "latinos"
team = "Team"
rank = "Rk"
notsolve = "--"
solv = "Slv."
pen = "Time"
-----------------------------------------------------------



-- Appl: appl es un procedimiento simple utilizado para leer información de un
--       texto en formato html. Primero crea el árbol XML correspondiente al texto,
--       luego trabaja con él usando la función f que le pasamos y luego te devuelve
--       te devuelve una lista con los valores que cumplen ciertas propiedades en el árbol. 
--       Si usamos funciones apropiadas, es muy simple sacar la información del árbol.
appl :: IOSArrow XmlTree c -> IO [c]
appl f = do
  html' <- readFile htmlIn
  let doc = readString [withParseHTML yes, withWarnings no] html'
  runX $ doc >>> f

-- tablaStand: Devuelve el sub-arbol XML que tiene como raíz la tabla correspondiente a los Standings
tablaStand :: ArrowXml a => a XmlTree XmlTree
tablaStand = deep (hasAttrValue "id" (==standings))


-- valuesPorFila: Celdas que tiene cada fila de la tabla
valuesPorFila :: IO [Int]
valuesPorFila = do p <- appl $ tablaStand //> ((hasName tr >>> getName) &&& (((getChildren >>> getName)) >. arr length))
                   return $ map snd p
              
-- headers: Encabezados, con su espacio ocupado                
headers :: IO [(String,String)]
headers = appl $ tablaStand //> (hasName "th"  >>> (deepest getText) &&&  (getAttrValue "colspan")) 

----------------------------------------------------------------------
-- Funciones Auxiliares:
--    Cantidad de valores Name, Text y Tipo de cada celda, usados para deducir que información 
--       que información le pertenece a cada celda

valuesNamePorCelda :: IO [Int]
valuesNamePorCelda = do p <- appl $ tablaStand //> (((hasName "td" <+> hasName "th") >>> getName) &&& (((getChildren >>> getName)) >. arr length))
                        return $ map snd p

valuesTextPorCelda :: IO [Int]
valuesTextPorCelda = do p <- appl $ tablaStand //> (((hasName "td" <+> hasName "th") >>> getName) &&& (((getChildren >>> getText)) >. arr length))
                        return $ map snd p

valueTypePorCelda :: IO [String]
valueTypePorCelda = do p <- appl $ tablaStand //> ((hasName "td" <+> hasName "th") >>> getName)
                       return p

inferValuesPorNameAndTextAndType :: String -> Int -> Int -> Int
inferValuesPorNameAndTextAndType "td" 0 _ = 0
inferValuesPorNameAndTextAndType _ 3 _ = 4
inferValuesPorNameAndTextAndType _ a b = max a b

-- Valores útiles que tiene cada celda
valuesPorCelda :: IO [Int]
valuesPorCelda = do ty <- valueTypePorCelda
                    vt <- valuesTextPorCelda
                    vn <- valuesNamePorCelda                    
                    return $ zipWith3 inferValuesPorNameAndTextAndType ty vt vn

-- Textos de las celdas
infoCeldas :: IO [String]
infoCeldas = do p <- appl $ tablaStand //> (hasName "td" <+> hasName "th") >>> (getName &&& deepest getText)
                return $ map snd p
-------------------------------------------------------------------------
-- celdas: Devuelve una lista de listas de String, cada lista de String corresponde a los textos de cada celda
celdas :: IO [Celda]
celdas = do v <- valuesPorCelda
            i <- infoCeldas
            return . reverse . snd $ foldl (\(acc,c) x -> (drop x acc, take x acc : c)) (i,[]) v 


-- tabla: Devuelve una lista de listas de celdas, cada lista corresponde a una fila.
tabla :: IO [Fila]
tabla = do c <- celdas
           v <- valuesPorFila
           return . reverse . snd $ foldl (\(acc,c) x -> (drop x acc, take x acc : c)) (c,[]) v

-- tablaTeams: Filtra la información no útil de la tabla, toma filas hasta 
--               que la primer celda es vacía       
tablaTeams :: IO [Fila]
tablaTeams = do t <- tabla
                return $ takeWhile (not . null . head) (drop 1 t)


-- teamsDeLatinoamerica: Conjuntos de nombres de los teams de Latinoamérica, usadas para remarcarlos 
--                           en el scoreboard que vamos a generar.
teamsDeLatinoamerica :: IO [String]
teamsDeLatinoamerica = do ts <- appl $ css tr //> (hasAttrValue title (==latTeams) <+> hasAttrValue title (==latamChampion)) >>> deepest getText 
                          return $ eliminarEspacios ts

-- strToProblem: Dada una celda, la transforma a formato Problema, un Maybe que tiene la información 
--                  de cuantos intentos hizo el equipo y cuántos minutos tardo en resolverlo.
--               Por los objetivos de este scoreboard, y la imposibilidad de saber en que mínuto fue 
--                  intentado erróneamente un problema, simplificamos y definimos de la misma manera
--                  un problema no resuelto que uno no intentado.
strToProblem :: [String] -> Problem
strToProblem [] = NotTried
strToProblem [xs,"--"] = Tried $ read xs 
strToProblem [xs,ys] = Solved (read xs,read ys)
strToProblem [xs,_,ys,_] = Solved (read xs,read ys)

-- strToTeam: Dada una fila de la tabla de los teams (representada como una lista de listas de String), e información
--               sobre en que columna está cada dato, devuelve el team en un formato más simple de trabajarlo.
strToTeam :: Int -> Int -> Int -> Fila -> Team
strToTeam ct fp cp xs = do let probs = take cp $ drop fp xs 
                           let np = map strToProblem $ map eliminarEspacios probs
                           Team {
                                name = head $ eliminarEspacios $ xs !! ct,
                                subs = map makeSubs np,
                                zone = Other,
                                user = ""
                                }

-- teams: Devuelve la lista de los teams, luego de leer información sobre el html.
teamsParseados :: IO [Team]
teamsParseados = do t <- tablaTeams
                    ct <- columnTeams
                    fp <- colFirstProblem
                    cp <- cantProblems
                    tl <- teamsDeLatinoamerica
                    let teamsWithoutZone = map (strToTeam ct fp cp) t
                    return $ map (\t -> if elem (name t) tl then (changeZone Latino t) else t) teamsWithoutZone




-------------------------------Funciones Auxiliares Usadas------------------------------------------------------
-- stoi: Transforma strings a int, usando un valor por defecto para cadenas vacias
stoi :: String -> Int -> Int  
stoi [] i = i 
stoi s _ = read s

stoiTuplaL :: Int -> [(a,String)] -> [(a,Int)]
stoiTuplaL = \i -> map (\(a,s) -> (a,stoi s i)) 

-- acumUntil: Función auxiliar, cuenta el acumulado de columnas. Será utilizado para saber
--                en que columna se encuentra un encabezado específico.
acumUntil :: (String -> Bool) -> [(String,Int)] -> Int 
acumUntil f xs = (scanl (+) 0 $ map snd xs) !! (fromJust $ findIndex f (map fst xs))

-- columnF: Devuelve el primer índice de la columna que cumple la restricción dada.
columnF :: (String -> Bool) -> IO Int
columnF f = do h <- headers
               return $ acumUntil f (stoiTuplaL 1 h)

columnTeams :: IO Int
columnTeams = columnF (==team)

columnRank :: IO Int
columnRank = columnF (==rank)

colFirstProblem :: IO Int
colFirstProblem = columnF (\p -> length p == 1)

columnSolved :: IO Int
columnSolved = columnF (==solv)

columnPenalization :: IO Int
columnPenalization = columnF (==pen)

-- Cant Problems: Función auxiliar que devuelve la cantidad de problemas
cantProblems :: IO Int 
cantProblems = do h <- headers 
                  let probs = map head . filter (\p -> length p == 1) $ map fst h
                  return $ numUniques probs        


