{-# LANGUAGE TemplateHaskell #-}
--      Module User
-- Este modulo se encargar de ofrecer funcionalidades extras al usuario, a fin de brindarle más opciones de cómo desea simular.
-- Provee funciones para agregar nuevos equipos, borrarlos, agregar nuevos envíos de los equipos, listar todos tus equipos, cambiar la velocidad de simulación, arrancar el simulacro, pausarlo, reanudarlo y reiniciarlo. 
-- Para hacer esto, trabaja modificando un estado general del contest (ContestState). Este estado es parte de la mónada general en forma de referencia y por lo tanto se usan funciones de IORef para modificarlo o accederlo.
module User where
import Application
import Data.List (intersperse)
import Data.Time.Clock
import Auxiliar
import Tipes
import Teams (crearTabla)
import Data.IORef (readIORef, atomicModifyIORef)
import Control.Monad.State (get, liftIO)
import Control.Lens.Setter (set)

 -- St es la monada Principal del Simulador

-- Lee el estado del scoreboard
getScore :: St ContestState
getScore = do
    g <- get
    q <- liftIO $ readIORef (_st g)
    return q

-- Modifica el estado del scoreboard dada una función.
modifyScore :: (ContestState -> ContestState) -> St ()
modifyScore f = do
    g <- get
    liftIO $ atomicModifyIORef (_st g) (\x -> (f x,()))
           
-- Funciones Auxiliares usadas para modificar una parte del estado del scoreboard
modifCP :: CantProblems -> St ()
modifCP x = modifyScore $ set cp x 

modifUT :: Teams -> St ()
modifUT x = modifyScore $ set ut x 

modifOT :: Teams -> St ()
modifOT x = modifyScore $ set ot x

modifTS :: TimeState -> St ()
modifTS x = modifyScore $ set ts x

modifV :: Int -> St ()
modifV x = modifyScore $ set v x

-- addOfitialTeams: Dado los teams oficiales, los carga en el estado.
addOfitialTeams :: Contest -> St ()
addOfitialTeams (cp,ot) = do 
            addOfitialTeams' cp ot
            modifTS Stop

addOfitialTeams' :: CantProblems -> Teams -> St ()
addOfitialTeams' cp ot = do 
                            modifCP $ cp
                            modifOT ot
                            modifV $ 60

-- changeVelocity: Altera la velocidad del scoreboard, solo si el contest no arrancó. Útil para analizar scoreboard sin participar. Por defecto es 60 (segundos).
changeVelocity :: Int -> St ()
changeVelocity v' = do 
                     st <- getScore
                     case _ts st of Stop -> modifV v'
                                    _ -> return ()

-- start inicializa un simulacro.
start :: St ()
start = do st <- getScore
           tm <- liftIO getCurrentTime 
           case _ts st of Stop -> modifTS $ Running tm 
                          _ -> return ()

-- stop frena el simulacro.
stop :: St ()
stop = do st <- getScore
          case _ts st of NotChoose -> return ()
                         _ -> modifTS Stop

-- pause pausa un simulacro, por si es necesario.
pause :: St ()
pause = do st <- getScore
           tp <- liftIO getCurrentTime 
           case _ts st of Running tm -> modifTS $ Pause tm tp 
                          _ -> return ()
 
-- unpause reanuda un simulacro, si éste está pausado, ignorando el tiempo pausado.
unpause :: St ()
unpause = do st <- getScore
             tu <- liftIO getCurrentTime 
             case _ts st of Pause tm tp -> modifTS $ Running $ addUTCTime (diffUTCTime tu tp) tm 
                            _ -> return ()

-- getMinute :: Devuelve el minuto actual del simulacro
getMinute :: St Int
getMinute = do
        m <- getMinute'
        return $ min 300 m

getMinute' :: St Int
getMinute' = do 
               st <- getScore
               t <- liftIO getCurrentTime
               case _ts st of Running tm -> return $ div (truncate $ diffUTCTime t tm) $ _v st
                              Pause tm tp -> return $ div (truncate $ diffUTCTime tp tm) $ _v st
                              _ -> return 0  

-- nameUsed :: Función Auxiliar. Dado un nombre, devuelve si ya está usado.
nameUsed :: String -> St Bool
nameUsed s = do st <- getScore
                let tt = (_ut st ++ _ot st)
                let f = filter (\t -> _name t == s) tt
                return $ null f        

-- auxUT :: Función auxiliar para simplificar el trabajo con los teams de los usuarios.
auxUT :: (a -> Int -> Teams -> Teams) -> a -> St ()
auxUT f x = do st <- getScore
               case _cp st of Nothing -> return ()
                              Just i -> modifUT $ f x i $ _ut st

-- addTeam :: Agrega un nuevo team (user, nombre de team)
addTeam :: (String, String) -> St ()
addTeam = auxUT addTeam'

addTeam' :: (String, String) -> Int -> Teams -> Teams
addTeam' (us,n) cp ts = let t = do
                                  let t = newTeam
                                  let t2 = set name n t
                                  let t3 = set subs (replicate cp []) t2
                                  let t4 = set zone User t3
                                  set user us t4
                        in t:ts

-- deleteTeam
deleteTeam :: (String, String) -> St ()
deleteTeam = auxUT deleteTeam'

deleteTeam' :: (String, String) -> Int -> Teams -> Teams
deleteTeam' (us,n) _ = filter (\t -> _name t /= n || _user t /= us)  

-- addSubmit (user, name, nroProb, Ac?, nroIntentos, minuto) agrega la submit a ese equipo.
addSubmit :: (String, String, Int, Bool, Int, Int) -> St ()
addSubmit = auxUT addSubmit'

-- addSubmit (user, name, nroProb, Ac?, min,  TotProb)
addSubmit' :: (String, String, Int, Bool, Int, Int) -> Int -> Teams -> Teams
addSubmit' (us,nomb,nprob,ac,tries,min) cp ts = map (\t -> if (_name t /= nomb || _user t /= us || cp < nprob) then t
                                                 else (do let ss = _subs t
                                                          let news = (ac,min) : replicate (tries-1) (False,min)   
                                                          let nss = replace (news ++ (ss!!(nprob-1))) (nprob-1) ss
                                                          set subs nss t)) ts

-- listTeams :: Lista los teams del usuario activo.
listTeams :: String -> St String
listTeams u = do
        st <- getScore
        return $ concat $ intersperse "\n" $ map (\t -> bool "" (_name t) ((_user t) == u)) (_ut st)

-- getContest :: Devuelve el simulacro actual, listo para ser mostrado
getContest :: St Tabla
getContest = do st <- getScore
                m <- getMinute
                return $ crearTabla (Just m,_ut st ++ _ot st)

