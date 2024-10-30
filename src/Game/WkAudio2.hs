module Game.WkAudio2(withWkAudio) where

import Sound.ALUT.Initialization (withProgNameAndArgs,runALUTUsingCurrentContext)
import Sound.ALUT.Loaders (createBuffer,SoundDataSource(File))
import Sound.OpenAL.AL.Buffer (Buffer)
import Sound.OpenAL.AL.Source (loopingMode,LoopingMode(..),queueBuffers)
import Sound.OpenAL.ALC.Device (openDevice,closeDevice)
import Sound.OpenAL.ALC.Context (createContext,currentContext)
import Data.ObjectName (genObjectName)
import Data.StateVar (($=))
import Control.Monad.IO.Class (MonadIO)
import Game.WkData (museRoot,museFiles)

withWkAudio :: (MonadIO m,MonadFail m) => ([Buffer] -> m a) -> m ()
withWkAudio op =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> 
   do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffers <- mapM createBuffer (map (\mf -> File (museRoot++mf++".wav")) museFiles)
    _ <- op buffers
    _ <- closeDevice device
    return ()

