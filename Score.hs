-- Modulo Score
--   Este modulo se encarga de la elección del scoreboard a simular.
--   Es la interacción entre el usuario y el modulo Parser que parsea
--      ese scoreboard. La comunicación se da mediante un archivo "in.html"

module Score where
import Utils
import Tipes
import Parser (htmlIn, teamsParseados)
import Network.HTTP


-- get: Dado un url, lee una página y la devuelve como String
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

-- grabarSimulacro: Dado un archivo en html y un path, parsea el scoreboard y lo graba para ser usado posteriormente.
grabarSimulacro :: Html -> Path -> IO ()
grabarSimulacro h p = do writeFile htmlIn h
                         t <- teamsParseados
                         writeFile  ("Scores/" ++ p ++ ".txt") $ show t

-- scanPage: Dado un url, y un nombre de archivo, scanea la página, la parsea y copia la información del simulacro al archivo.
scanPage :: Url -> Path -> IO ()
scanPage u p = do s <- get u
                  grabarSimulacro s p

-- scanFile: La misma función anterior pero para leerlo desde un archivo interno (formato html) de la carpeta Html.
scanFile :: Path -> Path -> IO ()
scanFile h p = do s <- readFile $ "Html/" ++ h ++ ".html"
                  grabarSimulacro s p

-- chooseScore: Dado un archivo de la carpeta Scores ya previamente cargado, 
--                        devuelve la lista de los teams que participaron.
chooseScore :: Path -> IO Teams
chooseScore p = do s <- readFile $ "Scores/" ++ p ++ ".txt"
                   return $ read s




