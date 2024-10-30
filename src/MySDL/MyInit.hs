module MySDL.MyInit(withMyInit) where

import SDL (quit)
import SDL.Init (initializeAll)
import qualified SDL.Font as F
import qualified SDL.Image as I 
import Control.Monad.IO.Class (MonadIO)

withMyInit :: MonadIO m => m a -> m ()
withMyInit op = do
  initializeAll
  F.initialize
  I.initialize []
  _ <- op
  quit
