-- Modulo Score
--   Este modulo se encarga de la elección del scoreboard a simular.
--   Es la interacción entre el usuario y el modulo Parser que parsea
--      ese scoreboard. La comunicación se da mediante un archivo "in.html"

module Score where
import Utils
import Parser (htmlIn)
import Network.HTTP

-- get: Dado un url, lee una página y la devuelve como String
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

-- scanPage: Dado un url, lee una página y la copia al archivo con el
--           que va a trabajar el scoreboard.
scanPage :: String -> IO ()
scanPage url = do s <- get url
                  writeFile htmlIn s

-- scanFile: Dado un path, lee un archivo y lo copia al archivo con el
--             que va a trabajar el scoreboard.
scanFile :: String -> IO ()
scanFile path = do s <- readFile path 
                   writeFile htmlIn s

-- elegirSimulacro: Elije un simulacro ya cargado.                 
elegirSimulacro :: Int -> IO ()
elegirSimulacro i | i == 2015 = scanFile $ "Scores/wf" ++ show i ++ ".html"
elegirSimulacro i = scanPage $ "http://static.kattis.com/icpc/wf" ++ show i ++ "/"
                       
