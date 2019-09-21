{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens (makeLenses)
import Snap.Snaplet
import Snap.Snaplet.Heist 
import Snap.Snaplet.Auth (AuthManager)
import Snap.Snaplet.Session (SessionManager)
import Data.IORef (IORef)
import Tipes
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _st :: IORef ContestState  
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type St = Handler App App
------------------------------------------------------------------------------
type AppHandler = Handler App App


