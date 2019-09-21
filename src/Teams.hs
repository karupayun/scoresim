{-# LANGUAGE TemplateHaskell #-}
-- Modulo Teams
-- Es el módulo lógico que trabaja con los equipos del simulacro.
-- Provee una única función que dada la información de los equipos y un minuto del simulacro, cálcula la información que debe ser mostrada en ese minuto, ocultando la información futura.
-- Lo que devuelve es la tabla ordenada de los equipos en ese minuto lista para ser mostrada.

module Teams (crearTabla) where
import Auxiliar
import Tipes
import Control.Lens (set, view)
import Data.List (sortBy)

-- countWA :: Dado un minuto, cuenta la cantidad de submission no aceptadas.
countWA :: Int -> Submissions -> Int
countWA m s = length $ filter (\(b,i) -> b == False && i <= m) s

-- subsProb :: Dado un minuto, calcula la situación de un problema dadas las submissions.
subsProb :: Int -> Submissions -> Problem
subsProb m s | m < firstSB = NotTried
             | m < firstAC = Tried $ countWA m s
             | otherwise = Solved (1+countWA firstAC s, firstAC)              
                where firstAC = foldl (\ac (b,i) -> if b then min ac i else ac) 301 s 
                      firstSB = foldl (\ac (_,i) -> min ac i ) 301 s 

-- probPoint :: Calcula el puntaje real de un problema
probPoint :: Problem -> (Int,Int)
probPoint NotTried = (0,0) 
probPoint (Tried _) = (0,0) 
probPoint (Solved (i,t)) = (1,t+20*i-20) 

-- teamPoint :: Dado un minuto, calcula el puntaje actual de este equipo.
teamPoint :: Int -> Submissionss -> (Int,Int)
teamPoint i ss = foldl1 addVectors $ map (probPoint . subsProb i) ss 

-- crearTabla :: Dado un minuto del simulacro y los equipos, lo transforma a forma mostrable y lo ordena según el formato usado para las competencias.
crearTabla :: Contest -> Tabla
crearTabla (Just i, ts) = let t = foldr (\t acc -> do
                                            let tp = teamPoint i $ view subs t
                                            let nt = newTeamLive
                                            let nt2 = set name2 (view name t) nt
                                            let nt3 = set solved (fst tp) nt2
                                            let nt4 = set penalization (snd tp) nt3
                                            let nt5 = set problems (map (subsProb i) $ view subs t) nt4
                                            let nt6 = set zone2 (view zone t) nt5
                                            let nt7 = set user2 (view user t) nt6
                                            nt7:acc) [] ts                                                             
                     in (i, zipWith (set position) [1..] $ sortBy better t)
       
