{-# LANGUAGE OverloadedStrings #-}
module Form where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Snaplet.Auth
--import           Snap.Core (writeText)
import Generator
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application
import           Score
import           State
import Control.Monad.Trans (liftIO)
data Datos = Datos {
  text1 :: T.Text,
  text2 :: T.Text
  --text3 :: T.Text,
  --int1 :: Int
} deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

textErrMsg :: T.Text
textErrMsg = "No puede haber un campo vacío"
intErrMsg :: T.Text
intErrMsg = "La variable debe ser entera"

form1 :: (Monad m) => Form T.Text m Datos
form1 = Datos
  <$> "text1" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "text2" .: text Nothing

form2 :: (Monad m) => Form T.Text m Datos
form2 = Datos
  <$> "text1" .: check textErrMsg isNotEmpty (text Nothing)
  <*> "text2" .: check textErrMsg isNotEmpty (text Nothing)

handleChoose :: Handler App App ()
handleChoose = do
  (view, result) <- runForm "choose" form1
  case result of
    Just x  -> do t <- liftIO $ chooseScore (T.unpack $ text1 x) 
                  addOfitialTeams t
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "chooseform"

handleNewContest :: Handler App App ()
handleNewContest = do
  (view, result) <- runForm "new_contest" form2
  case result of
    Just x  -> do liftIO $ scanFile (T.unpack $ text1 x) (T.unpack $ text2 x)
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "newcontestform"

-- usuarioActivo : Función que devuelve el usuario activo, o "" si no hay ninguno
usuarioActivo :: Handler App (AuthManager App) String
usuarioActivo = do
            us <- currentUser
            return $ maybe "" (T.unpack . userLogin) us

handleNewTeam :: Handler App (AuthManager App) ()
handleNewTeam = do
  (view, result) <- runForm "new_team" form1
  us <- usuarioActivo
  case result of
    Just x  -> do addTeam (us, (T.unpack $ text1 x))
                  redirect "/"
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "new_team"

handleStart :: Handler App App ()
handleStart = start >> redirect "/"

handleStop :: Handler App App ()
handleStop = stop >> redirect "/"

handlePause :: Handler App App ()
handlePause = pause >> redirect "/"

handleUnpause :: Handler App App ()
handleUnpause = unpause >> redirect "/"

handleScore :: Handler App App ()
handleScore = do 
            c <- getContest
            g <- liftIO $ generate c
            writeText $ T.pack $ g

--tweetForm :: (Monad m) => Form T.Text m Tweet
--tweetForm = Tweet
--  <$> "username" .: check userErrMsg isNotEmpty (text Nothing)
 -- <*> "timestamp" .: stringRead tsErrMsg Nothing
 -- <*> "content" .: check contentErrMsg isNotEmpty (text Nothing)

--tweetFormHandler :: Handler App App ()
--tweetFormHandler = do
--  (view, result) <- runForm "tweet" tweetForm
--  case result of
 --   Just x  -> writeText $ T.pack $ show $ 2 * timestamp x
  --  Nothing -> heistLocal (bindDigestiveSplices view) $ render "tweetform"
