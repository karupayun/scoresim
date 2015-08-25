-- Modulo Threads:
--      Se encarga de los crear y detener los hilos que van a generar el scoreboard
--          para cada minuto de tiempo. 
module Threads where
import Control.Concurrent 

cte :: Int
cte = 1000000

-- runAfterDelay: 
--      Dado una cantidad de minutos de tiempo y una función que realiza Entrada/Salida, 
--         se encarga de realizar la función luego de esperar t minutos.           
runAfterDelay :: Int -> IO () -> IO ThreadId -- Probar con una constante mayor para ver si funciona
runAfterDelay t f | t <= maxTime = forkIO (threadDelay (t*cte) >> f)
                  | otherwise = runAfterDelay (t-maxTime) (threadDelay (maxTime*cte) >> f)
                  where maxTime = div maxBound cte

-- stopThreads: 
--      Dado una lista de threads, los mata a todos 
stopThreads :: [ThreadId] -> IO ()
stopThreads ts = do sequence $ map killThread ts
                    return ()



