module Game.WkLoad (wkLoad) where

import qualified SDL.Image as I
import SDL.Video.Renderer (Surface)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import System.Directory (doesFileExist)
import MyFile (fileRead)
import Game.WkData (mapRoot,charaRoot,blockRoot,objectRoot,enemyRoot)

wkLoad :: MonadIO m => m [[Surface]]
wkLoad = do
  surfs <- mapM loadImages [mapRoot,charaRoot,enemyRoot,objectRoot,blockRoot]
  return surfs 

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

