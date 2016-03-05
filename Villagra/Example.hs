{-# LANGUAGE TemplateHaskell #-}
import Snap
import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text)
import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap.Snaplet.Heist

data Foo = Foo
data Bar = Bar
data App = App
     { _foo                :: Snaplet Foo
     , _bar                :: Snaplet Bar
     , _someNonSnapletData :: Text
     }
data App2 = App2
     { _foo2                :: Snaplet Foo
     , _bar2                :: Snaplet Bar
     , _companyName :: String
     }

makeLenses ''App
makeLenses ''App2

myHandler :: Handler App App ()
myHandler = do
  snaplet <- getSnapletState
  let val = view (snapletValue . someNonSnapletData) snaplet
  writeText val



myHandler2 :: Handler App2 App2 ()
myHandler2 = do
  a <- getSomeNonSnapletValueSomehow
  writeJSON [a]

companyHandler :: Handler App App ()
companyHandler = method GET getter <|> method POST setter
  where
    getter = do
        nameRef <- gets _companyName
        name <- liftIO $ readIORef nameRef
        writeBS name
    setter = do
        mname <- getParam "name"
        nameRef <- gets _companyName
        liftIO $ maybe (return ()) (writeIORef nameRef) mname
        getter
