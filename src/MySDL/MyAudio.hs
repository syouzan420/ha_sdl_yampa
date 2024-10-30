module MySDL.MyAudio(withMyAudio) where

import qualified SDL.Mixer as M
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

withMyAudio :: MonadIO m => m a -> m ()
withMyAudio op = do
  M.openAudio M.defaultAudio 1024
--  M.load (head musicFiles) >>= M.playMusic M.Forever
  void op
  M.closeAudio
  M.quit
