{-# LANGUAGE TemplateHaskell #-}
-- Modulo Tipos
-- Tipos que son usados a lo largo del simulador.

module Tipes where
import Data.Time
import Control.Lens
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

data TeamLive = TeamLive { name2 :: String, -- Información de un equipo durante una competencia
                     solved :: Int,
                     penalization :: Int,
                     problems :: Problems,
                     zone2 :: Zone,
                     position :: Int,
                     user2 :: String
               } deriving (Show, Read)
data TimeState = NotChoose | Stop | Running UTCTime | Pause UTCTime UTCTime deriving (Show, Read)
type OfitialTeams = Teams
type UserTeams = Teams
type Delay = Int
data ScoreState = ScoreState 
               { _cp :: CantProblems
               , _ut :: UserTeams
               , _ot :: OfitialTeams
               , _ts :: TimeState
               , _dl :: Delay
               } deriving (Show, Read)
type CantProblems = Maybe Int
type Minute = Int
type Teams = [Team] 
type TeamsLive = [TeamLive] 
type Contest = (CantProblems, Teams) 
type Tabla = (Minute, TeamsLive) 
type Path = String
type Url = String
type Html = String
makeLenses ''ScoreState


