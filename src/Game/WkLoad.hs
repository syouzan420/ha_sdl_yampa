module Game.WkLoad (wkLoad) where

import qualified SDL.Image as I
import qualified SDL.Font as F
import SDL.Video.Renderer (Surface)
import Control.Monad.IO.Class (MonadIO,liftIO)
import System.Directory (doesFileExist)
import Game.WkData (mapRoot,charaRoot,blockRoot,objectRoot,enemyRoot
                   ,fontSize,fontFiles)

wkLoad :: MonadIO m => m ([F.Font],[[Surface]])
wkLoad = do
  fonts <- loadFonts fontSize fontFiles
  surfs <- mapM loadImages [mapRoot,charaRoot,enemyRoot,objectRoot,blockRoot]
  return (fonts,surfs) 

loadFonts :: MonadIO m => F.PointSize -> [FilePath] -> m [F.Font]
loadFonts fs = mapM (`F.load` fs)

loadImages :: MonadIO m => FilePath -> m [Surface]
loadImages = loadImages' 0

loadImages' :: MonadIO m => Int -> FilePath -> m [Surface]
loadImages' i fileRoot = do
  let fileName = fileRoot ++ show i ++ ".png"
  ife <- liftIO$doesFileExist fileName
  if ife then do
    sf <- I.load fileName
    surfs <- loadImages' (i+1) fileRoot
    return (sf:surfs)
         else return []

