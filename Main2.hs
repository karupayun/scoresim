-- Module Main
--    Modulo general del simulador. Se encarga de la interacción con el usuario
--       y de llevar un estado, donde se encuentran la lista de equipos y la lista
--       de ids de hilos que se están ejecutando actualmente.

module Main where
import Data.Char (ord, toUpper) 
import Generator (generate)
import Threads (runAfterDelay,stopThreads)
import Teams (tableTime)
import Utils
import Tipes
import Score 
import State 
import Control.Concurrent 

type Delay = Int
type Step = Int
type Init = Int
type Args = (Init,Step,Delay)
type Id = Int

-- startSimulacro: Dado el minuto inicial, los segundos entre minutos del simulacro y el delay previo, empieza el simulacro. 
--                    El tiempo (en segundos) mínimo entre cada actualización será de 5 segundos.
startSimulacro :: Args -> IO [ThreadId] 
startSimulacro (i,s,d) = sequence $ map (\n -> runAfterDelay (n*(max 5 s)+d) $ imprimirTabla (n+i)) [0..(300-i)]

-- imprimirTabla: Dado un minuto, imprime la tabla en ese minuto
imprimirTabla :: Int -> IO ()
imprimirTabla i = do t <- tableTime i
                     generate t i

-- State: Tipo de datos que simboliza al estado del simulacro, lista de teams propios y lista de hilos activos.
data State = Init | St ([Team],[ThreadId]) 
                  | Stop State 
                  | Start Args State 
                  | AddTeam String State 
                  | DeleteTeam Int State 
                  | AddSubmit Id Submit State 
                  | GetPage String State 
                  | ChooseYear Int State 
                  | ChooseFile String State deriving Show
                  
stop :: State -> State
stop = Stop 

start :: Args -> State -> State
start = Start

addTeam :: String -> State -> State
addTeam = AddTeam

deleteTeam :: Int -> State -> State
deleteTeam = DeleteTeam

getPage :: String -> State -> State
getPage = GetPage

chooseYear :: Int -> State -> State
chooseYear = ChooseYear

chooseFile :: Int -> State -> State
chooseFile = ChooseYear

addWA :: Int -> Char -> Int -> State -> State
addWA id prob tries = AddSubmit id (prob,tries,301)

addAC :: Int -> Char -> Int -> Int -> State -> State
addAC id prob tries min = AddSubmit id (prob,tries,min)

runState :: State -> IO (State)
runState Init = return $ St ([],[])
runState (St x) = return $ St x
runState (Stop st') = do (St st) <- runState st'
                         stopThreads $ snd st 
                         return $ St (fst st, [])
runState (Start args st') = do (St st) <- runState st'
                               ti <- if (null $ snd st) then (startSimulacro args) else (return $ snd st)
                               return $ St (fst st, ti)
runState (AddTeam s st') = do (St st) <- runState st'
                              nt <- addTeam' s (fst st)
                              return $ St (nt, snd st)
runState (DeleteTeam i st') = do (St st) <- runState st'
                                 nt <- deleteTeam' i (fst st)
                                 return $ St (nt, snd st)
runState (AddSubmit id subm st') = do (St st) <- runState st'
                                      nt <- addSubmit' id subm (fst st)
                                      return $ St (nt, snd st)  
runState (GetPage s st) = do scanPage s
                             return st
runState (ChooseYear i st) = do elegirSimulacro i
                                return st
runState (ChooseFile s st) = do scanFile s
                                return st

-- Parser de la entrada del usuario
parse :: [String] -> State -> IO ()
parse ["stop"] st = exec $ Stop st
parse ["go"] st  = exec $ Start (0,60,10) st
parse ("start":s:s2:s3:[]) st = exec $ Start (read s,read s2,read s3) st
parse ("addTeam":xs) st = exec $ AddTeam (unwords xs) st
parse ("deleteTeam":s:[]) st = exec $ DeleteTeam (read s) st
parse ("addWA":id:prob:tries:[]) st = exec $ AddSubmit (read id) (head prob,read tries,301) st
parse ("addAC":id:prob:tries:minute:[]) st = exec $ AddSubmit (read id) (head prob,read tries,read minute) st
parse ["finish"] st = do runState $ Stop st
                         return ()
parse ("getPage":s:[]) st = do runState $ GetPage s st
                               exec st
parse ("chooseYear":s:[]) st = do runState $ ChooseYear (read s) st
                                  exec st
parse ("chooseFile":s:[]) st = do runState $ ChooseFile s st
                                  exec st
parse xs st = do putStr $ unwords xs
                 putStrLn $ ": Comando Invalido"
                 scanf st                                                                         

-- Funciones auxiliares encargadas de la entrada/salida para el usuario.

pstart :: IO ()
pstart = do  
  putStrLn $ "  \"start\": Inicia el simulacro"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          init: Minuto en el que inicia el simulacro (0 por defecto)"
  putStrLn $ "          step: Segundos entre cada actualización (60 por defecto)"
  putStrLn $ "          delay: Segundos hasta que inicia el simulacro (10 por defecto)"
  putStrLn $ "              Por ejemplo: \"start 0 60 10\""
  putStrLn $ "  \"go\": Inicia el simulacro por defecto, equivale a start 0 60 10"

pstop :: IO ()
pstop = do 
  putStrLn $ "Ingrese \"stop\" para frenar la simulación: "
  putStrLn $ "  Toda la información de los teams y las submisións "
  putStrLn $ "  quedara guardada. "
             
pteams :: IO ()
pteams = do 
  putStrLn $ "  \"addTeam\": Agregar un equipo"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          name: Nombre del equipo"
  putStrLn $ "              Por ejemplo: \"addTeam Caloventor en Dos\""
  putStrLn $ "  \"deleteTeam\": Elimina un equipo"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          id: Id del equipo"
  putStrLn $ "              Por ejemplo: \"deleteTeam 0\""
  putStrLn $ "  \"addAC\": Agrega una submission aceptada"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          id: id del equipo"
  putStrLn $ "          letter: letra del problema"
  putStrLn $ "          tries: intentos incluyendo el aceptado"
  putStrLn $ "          minuto: minuto del aceptado"
  putStrLn $ "              Por ejemplo: \"addAC 0 A 1 140\" agrega un problema acepted"
  putStrLn $ "                           en el primer intento, submiteado en el minuto 140"
  putStrLn $ "  \"addWA\": Agrega una submission rechazada"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          id: id del equipo"
  putStrLn $ "          letter: letra del problema"
  putStrLn $ "          tries: intentos incluyendo el aceptado"
  putStrLn $ "              Por ejemplo: \"addWA 0 A 2\" agrega un problema que tuvo 2 rechazos"

pchoose :: IO ()
pchoose = do
  putStrLn $ "  \"getPage\": Lee un html de una página para usarlo en el simulacro"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          st: url de la página"
  putStrLn $ "              Por ejemplo: \"getPage http://static.kattis.com/icpc/wf2013/\""
  putStrLn $ "  \"chooseYear\": Selecciona una world finals ya existente"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          year: año de la competencia (2012 a 2015)"
  putStrLn $ "              Por ejemplo: \"chooseYear 2015\""
  putStrLn $ "  \"chooseFile\": Selecciona un scoreboard interno para ser simulado"
  putStrLn $ "      Debe tener los siguientes argumentos: "
  putStrLn $ "          path: url del archivo"
  putStrLn $ "              Por ejemplo: \"chooseFile /home/pablo/tp/test2012.html\""
  
pfinish :: IO ()
pfinish = do 
   putStrLn $ "  \"finish\": Para finalizar el programa."
   putStrLn $ "      Este comando termina la simulación."  

scanf :: State -> IO ()
scanf st = do 
  putStrLn $ "Comando: "
  rta <- getLine
  parse (words rta) st

-- exec: Función principal. Dado un estado, lo ejecuta y realiza la entrada salida.  
exec :: State -> IO ()
exec st = do
  (St st') <- runState st
  putStrLn $ "Ingrese algún comando: "
  if (null $ snd st') then pstart else pstop 
  pteams
  pfinish
  scanf (St st')

-- main: Función inicial. Imprime un mensaje, limpia myTeams.txt y le pide al usuario que escoja un simulacro.
main = do
  putStrLn $ "Bienvenido al simulador de Scoreboard"
  writeFile "myTeams.txt" ""
  putStrLn $ "Elija el simulacro: "
  pchoose
  scanf Init
