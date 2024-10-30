module MyFile(fileRead,fileWrite) where

import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Data.Functor((<&>))

fileRead :: (MonadIO m) => FilePath -> m T.Text 
fileRead fileName = liftIO $ B.readFile fileName <&> decodeUtf8 

fileWrite :: (MonadIO m) => FilePath -> T.Text -> m ()
fileWrite fileName txt = liftIO $ B.writeFile fileName (encodeUtf8 txt)

