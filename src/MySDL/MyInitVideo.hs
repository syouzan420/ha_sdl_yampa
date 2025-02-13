module MySDL.MyInitVideo (withMyVideo,getImageSize) where

import SDL.Video (createWindow, defaultWindow, windowInitialSize
                 ,createRenderer,defaultRenderer,destroyWindow)
import SDL.Video.Renderer (Surface,Renderer,Texture,TextureInfo(..)
                          ,createTextureFromSurface,present,freeSurface
                          ,queryTexture)
import SDL.Vect (V2(..))
import MySDL.MyDraw (initDraw)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import MyData (Size,windowSize,title)

withMyVideo :: (MonadIO m) => [Surface] -> ((Renderer,[Texture]) -> m a) -> m ()
withMyVideo imageS op = do
      window <- createWindow title (defaultWindow {windowInitialSize = windowSize})
      renderer <- createRenderer window (-1) defaultRenderer
      itexs <- mapM (createTextureFromSurface renderer) imageS
      mapM_ freeSurface imageS
      initDraw renderer
      present renderer
      void $ op (renderer,itexs)
      destroyWindow window

getImageSize :: (MonadIO m) => [Texture] -> m [Size]
getImageSize = mapM (\t -> do (TextureInfo _ _ twd thi) <- queryTexture t 
                              return (V2 twd thi))
