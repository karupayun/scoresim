{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Control.Monad.Trans
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I


--import            Control.Monad
import            Control.Monad.State
--import            Data.ByteString.Char8 (ByteString)
--import qualified  Data.ByteString.Char8 as BS
--import            Data.Lens.Template
--import            Data.Map ((!))
--import            Data.Maybe
--import            Data.String
--import            Database.HDBC.Sqlite3
--import            Snap.Core
--import            Snap.Snaplet
--import            Snap.Snaplet.Auth
--import            Snap.Snaplet.Auth.Backends.Hdbc
--import            Snap.Snaplet.Hdbc
--import            Snap.Snaplet.Session
--import            Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------
import           Application
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

--help :: AuthManager App -> App
--help x = q <- 


--with auth :: Handler App (AuthManager App) () ->  Handler App App ()
--getSnapletState :: Handler App x (Snaplet x)
--auth :: (Snaplet (AuthManager App) -> f (Snaplet (AuthManager App)))
--     -> App -> f App
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r 

--test :: Handler App (AuthManager App) () -
------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do  liftIO $ print "hola"    
                    --    stat <- (with auth)
                        stat2 <- getSnapletState                        
           --             let val = view (snapletValue . st) stat2
            --            liftIO $ print $ val
                        loginUser "login" "password" Nothing
                         (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
-- | Imprimir State
printState :: Handler App App ()
printState = do
    stat <- getSnapletState
    let val = view (snapletValue . st) stat
    conn <- get
    liftIO $ print val
  
funcState :: Handler App App ()
funcState = do
    printState
    putState "Chau"
    printState
    putState "Si esto anda, soy feliz"
    printState
------------------------------------------------------------------------------
-- | get State
getState :: Handler App App String
getState = do
    stat <- getSnapletState
    let val = view (snapletValue . st) stat
    return val

------------------------------------------------------------------------------
-- | putState
putState :: String -> Handler App App ()
putState s = modify (changeOnlyString s)

------------------------------------------------------------------------------
--  | Change State
changeState2 :: Snaplet App -> Handler App App ()
changeState2 = putSnapletState

--changeState :: String -> Snaplet App -> Snaplet App
--changeState s a = do
--       snapletValue (\x -> (changeOnlyString s x)) a


idState :: Handler App App ()
idState = do
    stat <- getSnapletState
    putSnapletState stat

changeOnlyString :: String -> App -> App
changeOnlyString s a = App {
              _heist = _heist a
            , _sess = _sess a
            , _auth = _auth a
            , _st = s
            , _st2 = _st2}

changeOnlyString2 :: String -> App -> IO App
changeOnlyString2 s a = App {
              _heist = _heist a
            , _sess = _sess a
            , _auth = _auth a
            , _st = s
            , _st2 = _st2}

------------------------------------------------------------------------------
--  | Change String
changeString :: String -> Handler App App ()
changeString s = do
    stat <- getSnapletState -- stat es Snaplet App
--    let q = (liftSnap (changeOnlyString s) stat) --  
    return ()

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/state", funcState)
         , ("/state2", funcState)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    let st = "Hola"
    return $ App h s a st

