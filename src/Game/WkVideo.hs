module Game.WkVideo (withVideo) where

import SDL.Video (createWindow, defaultWindow, windowInitialSize
                 ,createRenderer,defaultRenderer,destroyWindow)
import SDL.Video.Renderer (Renderer,Texture,present,createTexture,TextureAccess(..),PixelFormat(..))
import SDL.Vect (V2(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import MySDL.MyDraw (initDraw)
import Game.WkData (windowSize,title)

withVideo :: (MonadIO m) =>  ((Renderer,Texture) -> m a) -> m ()
withVideo op = do
      window <- createWindow title (defaultWindow {windowInitialSize = windowSize})
      renderer <- createRenderer window (-1) defaultRenderer
      initDraw renderer
      present renderer
      texture <- createTexture renderer ARGB8888 TextureAccessTarget (V2 1 1)
      void $ op (renderer,texture)
      destroyWindow window
