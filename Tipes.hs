-- Modulo Tipos
-- Tipos que son usados a lo largo del simulador.

module Tipes where
data Problem = Tried Int | Solved (Int,Int) | NotTried deriving (Read,Show)
type Problems = [Problem]
type Submissions = [(Bool, Int)]
type Submissionss = [Submissions]
data Zone = Latino | User | Other deriving (Eq,Show,Read)
data Team = Team { name :: String, -- Información de un equipo
                   subs :: [Submissions],
                   zone :: Zone,
                   user :: String
               } deriving (Read, Show)

data Team2 = Team2 { name2 :: String, -- Información de un equipo durante una competencia
                     solved :: Int,
                     penalization :: Int,
                     problems :: Problems,
                     zone2 :: Zone,
                     position :: Int,
                     user2 :: String
               } --deriving Show
type CantProblems = Maybe Int
type Minute = Int
type Teams = [Team] 
type Teams2 = [Team2] 
type Contest = (CantProblems, Teams) 
type Tabla = (Minute, Teams2) 
type Path = String
type Url = String
type Html = String
