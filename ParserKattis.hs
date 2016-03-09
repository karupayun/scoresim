{-# LANGUAGE TemplateHaskell #-}
-- Modulo ParserKattis
--   Ante los múltiples formatos de los scoreboards, se necesita un parser particular para cada tipo.
--   A modo de ejemplo, se provee el parser utilizado para leer los scoreboards utilizados en las competencias mundiales (que lo vamos a llamar el "formato Kattis"). Para poder leer otros scoreboard generalmente se necesitan cambios menores (modificar algunas constantes y en algún que otro caso alguna otra función minimamente). El módulo usa una librería de HTML llamada HXT para parsear el código HTML.
--   Como aclaración sobre este modulo, esto fue lo primero que realizé (hace casi 1 año) muy rudimentariamente y es una parte que yo   considero como menor dentro del trabajo completo. El primer cambio a futuro que considero hacer es trabajar sobre este módulo, haciéndolo todavía más reusable y refórmandolo por completo (usa un archivo externo en lugar de una mejor mónada). Queda como trabajo pendiente.

module ParserKattis (teamsParseados) where
import Tipes 
import Auxiliar (makeSubs, newTeam)
import Text.XML.HXT.Core
import Text.HandsomeSoup (css)
import Data.List (findIndex, nub)
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Control.Lens (set)

type Celda = [String]
type Fila = [Celda]


--Constantes del Parser, sirven para hacer el código más reusable ante 
--  la diversidad de formatos distintos de scoreboards
htmlIn = "Internos/in.html" -- TODO: Desaparecer esto, guardando el string como parte del estado.
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
--       que información le pertenece a cada celda. Ante lo rebuscado del scoreboard de kattis, 
--       esto fue lo más simple que encontré para deducir la información de cada celda.

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

-- teamsDeLatinoamerica: Conjuntos de nombres de los teams de Latinoamérica, usadas para remarcarlos 
--                           en el scoreboard que vamos a generar.
teamsDeLatinoamerica :: IO [String]
teamsDeLatinoamerica = do ts <- appl $ css tr //> (hasAttrValue title (==latTeams) <+> hasAttrValue title (==latamChampion)) >>> deepest getText 
                          return $ eliminarEspacios ts


-----------------------------------------------------------------------------
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
                           let nt = newTeam
                           let nt' = set name (head $ eliminarEspacios $ xs !! ct) nt   
                           set subs (map makeSubs np) nt'

-- teams: Devuelve la lista de los teams, luego de leer información sobre el html.
teamsParseados :: Html -> IO Contest
teamsParseados h = do 
                 writeFile htmlIn h
                 t <- tablaTeams
                 ct <- columnTeams
                 fp <- colFirstProblem
                 cp <- cantProblems
                 tl <- teamsDeLatinoamerica
                 let teamsWithoutZone = map (strToTeam ct fp cp) t
                 return $ (Just cp, map (\t -> if elem (_name t) tl then (set zone Latino t) else t) teamsWithoutZone)


-------------------------------Funciones Auxiliares Usadas------------------------------------------------------

-- stoi: Transforma strings a int, usando un valor por defecto para cadenas vacias
stoi :: String -> Int -> Int  
stoi [] i = i 
stoi s _ = read s

stoiTuplaL :: Int -> [(a,String)] -> [(a,Int)]
stoiTuplaL = \i -> map (\(a,s) -> (a,stoi s i)) 

-- Elimina espacios anteriores y posteriores de una lista de Strings
eliminarEspacios :: [String] -> [String] 
eliminarEspacios = map (reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace))

-- Cantidad de valores únicos en una lista
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Cant Problems: Función auxiliar que devuelve la cantidad de problemas del simulacro
cantProblems :: IO Int 
cantProblems = do h <- headers 
                  let probs = map head . filter (\p -> length p == 1) $ map fst h
                  return $ numUniques probs  

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

      


