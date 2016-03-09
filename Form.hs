{-# LANGUAGE OverloadedStrings #-}
--      Module Form
-- Este módulo le provee al modulo Site todos los formularios para las distintas acciones que puede hacer el usuario. \\
-- Es la interacción entre lo que realiza el usuario y los modulos Score y User que realizan las funciones básicas de la simulación. \\
-- Usa una única estructura, la cuál usa según la cantidad de información que debe brindar el usuario para cada función.
module Form where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap (runForm)
import           Control.Applicative
import           Snap.Snaplet.Auth
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Control.Monad.Trans (liftIO)
import           Application
import           HtmlContestPrinter (generate)
import           Score
import           User

data DatosForm = DatosForm {
    text1 :: T.Text
  , text2 :: T.Text
  , int1 :: Int
  , int2 :: Int
  , int3 :: Int
  , int4 :: Int
} deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

textErrMsg :: T.Text
textErrMsg = "No puede haber un campo vacío"
intErrMsg :: T.Text
intErrMsg = "La variable debe ser entera"


-- form1 :: Lee un formulario con un solo campo de formato texto.
form1 :: (Monad m) => Form T.Text m DatosForm
form1 = DatosForm
  <$> "text1" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "text2" .: text Nothing
  <*> "int1" .: stringRead intErrMsg (Just 0)
  <*> "int2" .: stringRead intErrMsg (Just 0)
  <*> "int3" .: stringRead intErrMsg (Just 0)
  <*> "int4" .: stringRead intErrMsg (Just 0)

-- form2 :: Lee un formulario con dos campos de formato texto.
form2 :: (Monad m) => Form T.Text m DatosForm
form2 = DatosForm
  <$> "text1" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "text2" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "int1" .: stringRead intErrMsg (Just 0)
  <*> "int2" .: stringRead intErrMsg (Just 0)
  <*> "int3" .: stringRead intErrMsg (Just 0)
  <*> "int4" .: stringRead intErrMsg (Just 0)

-- formInt :: Lee un formulario un solo campo de formato entero.
formInt :: (Monad m) => Form T.Text m DatosForm
formInt = DatosForm
  <$> "text1" .: text Nothing
  <*> "text2" .: text Nothing
  <*> "int1" .: stringRead intErrMsg Nothing
  <*> "int2" .: stringRead intErrMsg (Just 0)
  <*> "int3" .: stringRead intErrMsg (Just 0)
  <*> "int4" .: stringRead intErrMsg (Just 0)

-- formInt :: Lee un formulario especial para las submissions.
formSubmit :: (Monad m) => Form T.Text m DatosForm
formSubmit = DatosForm
  <$> "text1" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "text2" .: text Nothing
  <*> "int1" .: stringRead intErrMsg Nothing
  <*> "int2" .: stringRead intErrMsg Nothing
  <*> "int3" .: stringRead intErrMsg Nothing
  <*> "int4" .: stringRead intErrMsg Nothing

-- handleChoose :: El Handler para elegir el simulacro.
handleChoose :: Handler App App ()
handleChoose = do
  (view, result) <- runForm "choose" form1
  case result of
    Just x  -> do t <- liftIO $ chooseScore (T.unpack $ text1 x) 
                  addOfitialTeams t
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "choose"

-- handleChargeFile :: El Handler para cargar un contest mediante un archivo.
handleChargeFile :: Handler App App ()
handleChargeFile = do
  (view, result) <- runForm "charge_file" form2
  case result of
    Just x  -> do liftIO $ scanFile (T.unpack $ text1 x) (T.unpack $ text2 x)
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "charge_file"

-- handleChargeWeb :: El Handler para cargar un contest mediante un archivo.
handleChargeWeb :: Handler App App ()
handleChargeWeb = do
  (view, result) <- runForm "charge_web" form2
  case result of
    Just x  -> do liftIO $ scanPage (T.unpack $ text1 x) (T.unpack $ text2 x)
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "charge_web"

-- handleVelocity :: El Handler para cambiar la velocidad de la simulación.
handleVelocity :: Handler App App ()
handleVelocity = do
  (view, result) <- runForm "delay" formInt
  case result of
    Just x  -> do changeVelocity (int1 x)
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "delay"

-- usuarioActivo : Función que devuelve el usuario activo, o "" si no hay ninguno
usuarioActivo :: Handler App App String
usuarioActivo = do
            us <- with auth currentUser
            return $ maybe "" (T.unpack . userLogin) us

-- handleNewTeam :: El Handler para agregar un nuevo equipo.
handleNewTeam :: Handler App App ()
handleNewTeam = do
  (view, result) <- runForm "new_team" form1
  us <- usuarioActivo
  case result of
    Just x  -> do addTeam (us, (T.unpack $ text1 x))
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "new_team"

handleDeleteTeam :: Handler App App ()
handleDeleteTeam = do
  (view, result) <- runForm "delete_team" form1
  us <- usuarioActivo
  case result of
    Just x  -> do deleteTeam (us, (T.unpack $ text1 x))
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "delete_team"

-- handleTeams :: El Handler que muestra tus equipos.
handleTeams :: Handler App App ()
handleTeams = do
    us <- usuarioActivo
    l <- listTeams us
    writeText $ T.pack l
       
-- handleNewSubmit :: El Handler para agregar una submission a un equipo.
handleNewSubmit :: Handler App App ()
handleNewSubmit = do
  (view, result) <- runForm "new_submit" formSubmit
  us <- usuarioActivo
  case result of
    Just x  -> addSubmit (us, (T.unpack $ text1 x), int1 x, int2 x /= 0, int3 x, int4 x)
               redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "new_submit"

-- handleStart :: El Handler para arrancar el simulacro.
handleStart :: Handler App App ()
handleStart = start >> redirect "/"

handleStop :: Handler App App ()
handleStop = stop >> redirect "/"

handlePause :: Handler App App ()
handlePause = pause >> redirect "/"

handleUnpause :: Handler App App ()
handleUnpause = unpause >> redirect "/"

-- handleScore :: El Handler que te muestra el scoreboard.
handleScore :: Handler App App ()
handleScore = do 
            c <- getContest
            g <- liftIO $ generate c
            writeText $ T.pack $ g

