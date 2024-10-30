module Game.WkMap(createMapS) where

import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Foreign.Storable (peek)
import qualified SDL.Raw.Types as SDLT
import SDL.Internal.Numbered (fromNumber)
import SDL.Vect (Point(P),V2(..))
import SDL.Video.Renderer (Surface,surfaceFormat,createRGBSurface,surfaceBlit
                          ,SurfacePixelFormat (..))
import Game.WkData (GMap,Pos)

createMapS :: (MonadIO m) => [Surface] -> GMap -> m Surface
createMapS sfs gm = do
  let (V2 msx msy) = V2 (fromIntegral (length (head gm))) (fromIntegral (length gm))
  SurfacePixelFormat ppf <- surfaceFormat (head sfs)
  spf <- liftIO $ peek ppf
  let pfm = fromNumber (SDLT.pixelFormatFormat spf)
  mapSurf <- createRGBSurface (V2 (32*msx) (32*msy)) pfm 
  copyMapSurfs sfs mapSurf gm (V2 0 0)
  return mapSurf

copyMapSurfs :: (MonadIO m) => [Surface] -> Surface -> GMap -> Pos -> m () 
copyMapSurfs _ _ [] _ = return ()
copyMapSurfs sfs msf (ln:xs) pos = do 
  copyLineSurfs sfs msf ln pos
  copyMapSurfs sfs msf xs (pos + V2 0 32) 

copyLineSurfs :: (MonadIO m) => [Surface] -> Surface -> String -> Pos -> m ()
copyLineSurfs _ _ [] _ = return ()
copyLineSurfs sfs msf (t:ts) pos = do 
  cpSurf (sfs!!readMap t) msf pos 
  copyLineSurfs sfs msf ts (pos + V2 32 0)

cpSurf :: (MonadIO m) => Surface -> Surface -> Pos -> m ()
cpSurf sf msf pos = do
  _ <- surfaceBlit sf Nothing msf (Just (P pos))
  return ()

readMap :: Char -> Int
readMap ch = fromMaybe 0 $ elemIndex ch "0123456789abcdefg"
  

