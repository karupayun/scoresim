{-# LANGUAGE TemplateHaskell #-}
-- Modulo Tipos
-- Tipos y sinónimos de tipos que son usados a lo largo del simulador.

module Tipes where
import Data.Time (UTCTime)
import Control.Lens (makeLenses)


data Problem = Tried Int | Solved (Int,Int) | NotTried deriving (Read,Show)
type Problems = [Problem]

type Submissions = [(Bool, Int)]
type Submissionss = [Submissions]

data Zone = Latino | User | Other deriving (Eq,Show,Read)

data Team = Team { -- Información de un equipo
                    _name :: String, 
                    _subs :: [Submissions],
                    _zone :: Zone,
                    _user :: String
               } deriving (Read, Show)
type Teams = [Team] 

data TeamLive = TeamLive { -- Información de un equipo durante una competencia
                     _name2 :: String, 
                     _solved :: Int, 
                     _penalization :: Int,
                     _problems :: Problems,
                     _zone2 :: Zone,
                     _position :: Int,
                     _user2 :: String
               } deriving (Show, Read)
type TeamsLive = [TeamLive] 

-- TimeState : Estado del estado del simulacro durante una competencia, con información del tiempo.
data TimeState = NotChoose | Stop | Running UTCTime | Pause UTCTime UTCTime deriving (Show, Read) 

-- Tiempo que tarda en actualizar el scoreboard al próximo minuto
type CantProblems = Maybe Int

data ContestState = ContestState 
               { _cp :: CantProblems
               , _ut :: Teams
               , _ot :: Teams
               , _ts :: TimeState
               , _v :: Int -- Velocidad del simulacro
               } deriving (Show, Read)



-- Contest :: Información de la competencia
type Contest = (CantProblems, Teams)

-- Tabla :: Tabla dado un minuto 
type Tabla = (Int, TeamsLive) 

-- Sinónimos de tipos para la lectura de competencias.
type Path = String
type Url = String
type Html = String

makeLenses ''ContestState
makeLenses ''Team
makeLenses ''TeamLive


