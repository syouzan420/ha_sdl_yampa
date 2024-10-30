module Game.WkMain (runWaka) where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Font (Font)
import SDL.Input.Keyboard (stopTextInput)
import Data.ObjectName (genObjectName)
import MyData (textFileName)
import MyFile (fileRead)
import Game.WkData (initWaka)
import Game.WkAction (startText)
import Game.WkVideo (withVideo)
import Game.WkAudio2 (withWkAudio)
import Game.WkLoop (wkLoop)
import Game.WkLoad (wkLoad)

type FileNum = Int

runWaka :: (MonadIO m,MonadFail m) => FileNum -> Text -> [Font] -> m () 
runWaka fln sIndex fonts = do 
  stopTextInput
  surfs <- wkLoad
  allText <- fileRead (textFileName++show fln++".txt")
  withVideo $ \(renderer,texture) -> do
    let newWaka = startText sIndex allText initWaka
    withWkAudio $ \muses -> do
      source <- genObjectName
      S.runStateT (wkLoop renderer fonts surfs texture muses source) newWaka
