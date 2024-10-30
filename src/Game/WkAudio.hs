module Game.WkAudio(withWkAudio) where

import qualified SDL.Mixer as M
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import Game.WkData (museRoot, museFiles)

withWkAudio :: MonadIO m => ([M.Music] -> m a) -> m ()
withWkAudio op = do
  M.openAudio M.defaultAudio 256 
  muses <- mapM M.load (map (\mf -> museRoot++mf++".mp3") museFiles)
  void $ op muses
  M.closeAudio
  M.quit
