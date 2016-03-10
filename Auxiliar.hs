{-# LANGUAGE TemplateHaskell #-}
-- Modulo Auxiliar
-- Funciones auxiliares útiles que son usadas a lo largo del simulador.

module Auxiliar where
import Tipes
import Data.List (length)
import Control.Lens

-- replace :: reemplaza un valor en una posición fija en una lista dejando los otros campos sin alterar.
replace :: a -> Int -> [a] -> [a]
replace item pos lista = do (take pos lista) ++ (item:drop (pos+1) lista)  

-- addVectors :: suma dos vectores
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) 
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- makeSubs :: inventa las posibles submissions dado la información final de un problema (sin tener la verdadera información).
makeSubs :: Problem -> Submissions
makeSubs NotTried = []
makeSubs (Tried i) = replicate (i) (False,301)
makeSubs (Solved (c,m)) = (True,m) : replicate (c-1) (False,m)

-- bool :: Función auxiliar para trabajar con Bools. La debería exportar Data.Bool pero no me la exporta.
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

-- newTeam :: Crea un nuevo Team
newTeam :: Team
newTeam = Team {
            _name = ""
          , _subs = []
          , _zone = Other
          , _user = ""
            }

-- newTeamLive :: Crea un nuevo TeamLive
newTeamLive :: TeamLive
newTeamLive = TeamLive {
            _name2 = ""
          , _solved = 0
          , _penalization = 0
          , _problems = []
          , _zone2 = Other
          , _position = 0
          , _user2 = ""
            }

-- vacio :: Scoreboard vacío
vacio :: ContestState
vacio = ContestState {_cp = Nothing, _ut = [], _ot = [], _ts = NotChoose, _v = 0}

-- better: compara dos teams
better :: TeamLive -> TeamLive -> Ordering
better t1 t2 | _solved t1 /= _solved t2 = compare (_solved t2) (_solved t1)
             | _penalization t1 /= _penalization t2 = compare (_penalization t1) (_penalization t2)
             | otherwise = compare (_name2 t1) (_name2 t2)

