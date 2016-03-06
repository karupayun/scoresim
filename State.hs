{-# LANGUAGE TemplateHaskell #-}
module State where
import Application
import Snap.Snaplet
import Data.Time.Clock
import Utils
import Tipes
import Teams (crearTabla)
import Data.Maybe
import           Snap.Snaplet.Auth
--import Control.Monad.Trans.State 
import Control.Monad.Trans (liftIO)
import Data.IORef 
import Control.Monad.State
import Control.Lens
--type St = StateT ScoreState IO
type St = Handler App App 
type St2 = Handler App (AuthManager App)


getScore :: St ScoreState
getScore = do
    g <- get
    q <- liftIO $ readIORef (_st g)
    return q

putScore :: ScoreState -> St ()
putScore s = do
    g <- get
    liftIO $ atomicWriteIORef (_st g) s

modifyScore :: (ScoreState -> ScoreState) -> St ()
modifyScore f = do
    g <- get
    liftIO $ atomicModifyIORef (_st g) (\x -> (f x,()))
           

modifyScore2 :: (ScoreState -> ScoreState) -> St2 ()
modifyScore2 f = do
    g <- get
    liftIO $ atomicModifyIORef (_st g) (\x -> (f x,()))
               


-- Cambiar el tipo a un record

modifCP :: CantProblems -> St ()
modifCP x = modifyScore (\st -> set cp x st) 

modifUT :: UserTeams -> St2 ()
modifUT x = modifyScore2 (\st -> set ut x st)

modifOT :: OfitialTeams -> St ()
modifOT x = modifyScore (\st -> set ot x st)

modifTS :: TimeState -> St ()
modifTS x = modifyScore (\st -> set ts x st)

modifDL :: Delay -> St ()
modifDL x = modifyScore (\st -> set dl x st)


--runStateT :: s -> m (a, s)
--newtype StateT s m a 

--newtype Op a = Op {runOp :: ScoreState -> IO (ScoreState, a)}
--liftIO :: IO a -> Op a
--liftIO io = Op $ \st -> do
 -- x <- io
 -- return (st, x)

--liftIO :: IO a -> St a
--liftIO io =  do x <- io
                
  --              put (st, x)

-- vacio :: ScoreState
vacio :: ScoreState
vacio = ScoreState {_cp = Nothing, _ut = [], _ot = [], _ts = NotChoose, _dl = 0}
    

-- initSimu: Inicializa el simulador.
initSimu :: St ()
initSimu = putScore vacio


addOfitialTeams' :: Int -> OfitialTeams -> St ()
addOfitialTeams' cp ot = do 
                            modifCP $ Just cp
                            modifOT ot



-- addOfitialTeams: Dado los teams oficiales, los carga.
addOfitialTeams :: OfitialTeams -> St ()
addOfitialTeams ot = do 
            addOfitialTeams' (cantProblemsFromTeam ot) ot
            modifTS Stop

-- changeDelay: Altera el delay entre actualizaciones, solo si el contest no arrancó. Útil para analizar scoreboard sin participar.
changeDelay :: Int -> St ()
changeDelay d' = do st <- getScore
                    case _ts st of Stop -> modifDL d'
                                   _ -> return ()

--execStateT :: Monad m => StateT s m a -> s -> m s 

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

getMinute :: St Int
getMinute = do
        m <- getMinute'
        return $ min 300 m

-- getMinute :: Devuelve el minuto actual del simulacro
getMinute' :: St Int
getMinute' = do 
               st <- getScore
               t <- liftIO getCurrentTime
               case _ts st of Running tm -> return $ div (truncate $ diffUTCTime t tm) $ 1
                              Pause tm tp -> return $ div (truncate $ diffUTCTime tm tp) $ _dl st
                              _ -> return 0  



-- getContest :: Devuelve el simulacro actual, listo para ser mostrado
getContest :: St Tabla
getContest = do st <- getScore
                m <- getMinute
                return $ crearTabla m (_ut st ++ _ot st)

-- nameUsed :: Dado un nombre, devuelve si ya está usado.
nameUsed :: String -> St Bool
nameUsed s = do st <- getScore
                let tt = (_ut st ++ _ot st)
                let f = filter (\t -> name t == s) tt
                return $ null f        

addTeam' :: (String, String) -> Int -> Teams -> Teams
addTeam' (us,n) _cp _ts = let t = Team{
                             name = n,
                             subs = replicate _cp [],
                             zone = User,
                             user = us
                            }
                        in t:_ts

deleteTeam' :: (String, String) -> Int -> Teams -> Teams
deleteTeam' (us,n) _ = filter (\t -> name t /= n || user t /= us)  


-- addSubmit (user, name, nroProb, Ac?, min,  TotProb)
addSubmit' :: (String, String, Int, Bool, Int) -> Int -> Teams -> Teams
addSubmit' (us,nomb,nprob,ac,min) _cp _ts = map (\t -> if (name t /= nomb || user t /= us || _cp < nprob) then t
                                                 else (do let ss = subs t
                                                          let nss = replace ((ac,min) : (ss!!(nprob-1))) (nprob-1) ss
                                                          modifSubs nss t)) _ts



-- addTeam :: Agrega un nuevo team (user, nombre de team)
addTeam :: (String, String) -> St2 ()
addTeam = auxUT addTeam'

--deleteTeam
deleteTeam :: (String, String) -> St2 ()
deleteTeam = auxUT deleteTeam'

-- addSubmit (user, name, nroProb, Ac?, minuto) agrega la submit a ese equipo.
addSubmit :: (String, String, Int, Bool, Int) -> St2 ()
addSubmit = auxUT addSubmit'


-- auxUT :: Función polimórfica para simplificar el trabajo con los teams de los usuarios.
auxUT :: (a -> Int -> Teams -> Teams) -> a -> St2 ()
auxUT f x = do st <- getScore
               case _cp st of Nothing -> return ()
                              Just i -> modifUT $ f x i $ _ut st

-- imprimirScoreboard :: State ScoreState ()
-- cleanSubmits :: Limpie todas las submits

