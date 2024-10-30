module Game.WkDraw (wkDraw) where

import Control.Monad (when,unless)
import Control.Monad.IO.Class (MonadIO,liftIO)
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Internal.Numbered (fromNumber)
import SDL.Video (Renderer)
import SDL.Video.Renderer (Surface,Texture,SurfacePixelFormat(..),PixelFormat(..)
                          ,Rectangle(..),copy
                          ,present,lockSurface,unlockSurface,surfacePixels
                          ,surfaceFormat,createRGBSurfaceFrom,createTextureFromSurface
                          ,destroyTexture)
import SDL.Font (Font)
import qualified SDL.Raw.Types as SDLT
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Storable (peek)
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Data.List (transpose)
import MySDL.MyDraw (initDraw,textsDraw)
import Game.WkData (Waka(..),Size,Pos,GMap,Direction(..),Cha(..),mapUpLeftPos,plDelay)
import Game.WkLib (cosList,shiftList)
import MyData (TextData,WMode(..))

type MapSize = Size
type TileSize = CInt
type PlayerNum = Int
type EffectNum = Int
type MapPos = Pos
type MapRPos = Pos

wkDraw :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> Texture 
                                                    -> TextData -> Waka -> m ()
wkDraw re fonts surfs texture textData wk = do
  let plnWk = pln wk  --player chara number
  let plyWk = chs wk!!plnWk
  let (mszWk,tszWk,mpsWk,mrpWk,acoWk,iscWk) = (msz wk,tsz wk,mps wk,mrp wk,aco wk,isc wk)
  initDraw re
  --unless (tmd wk==0) $ mapDraw2 re texture mszWk tszWk mpsWk mrpWk
  unless (tmd wk==0) $ mapDraw re (head surfs) (gmp wk) mszWk tszWk mpsWk acoWk
  when (ipl wk) $ do
      playerDraw re (surfs!!1) (tsz wk) plnWk plyWk mpsWk mrpWk iscWk acoWk
  textsDraw re fonts T True False (tps wk) textData
  present re

playerDraw :: (MonadIO m) => Renderer -> [Surface] -> 
                  TileSize -> PlayerNum -> Cha -> MapPos -> MapRPos ->Bool -> Int -> m ()
playerDraw re surfs tSize pn chaWk mpos mrps iss count = do 
  let (ps,rp,pd,pc) = (cps chaWk, crp chaWk, cdr chaWk,cac chaWk) 
      --enums = repeat (0 :: Int)
      pSurfs = take 8 $ drop (pn*8) surfs
      chDif = if pc < plDelay then 0 else 1
      chNum = case pd of
                South -> 0 + chDif 
                North -> 2 + chDif 
                East  -> 4 + chDif
                West  -> 6 + chDif 
      playerWindowPosition = mapUpLeftPos + V2 tSize tSize * (ps-mpos)
                                          + (if iss then V2 0 0 else rp)
                                          - (if iss then V2 0 0 else mrps)
  texture <- createTexture re (pSurfs!!chNum) 0 count 
  texDraw re texture tSize playerWindowPosition 
  destroyTexture texture

visibleGmap :: GMap -> MapSize -> Pos -> GMap
visibleGmap gmap (V2 mw mh) (V2 mx my) = 
  let mx' = fromIntegral mx
      my' = fromIntegral my
      mw' = fromIntegral mw
      mh' = fromIntegral mh
      mLines = take mh' $ drop my' gmap 
   in map (visibleMapLine mw' mx') mLines 

visibleMapLine :: Int -> Int -> String -> String
visibleMapLine mw mx mLine = take mw $ drop mx mLine

mapDraw2 :: MonadIO m => Renderer -> Texture -> MapSize -> TileSize 
                                              -> MapPos -> MapRPos -> m ()
mapDraw2 re tx mSize tSize mPos mRPos = do
  let source = mPos * V2 tSize tSize + mRPos
      width = mSize * V2 tSize tSize
  copy re tx (Just (Rectangle (P source) width))
             (Just (Rectangle (P mapUpLeftPos) width))


mapDraw :: (MonadIO m) => Renderer -> [Surface]
                         -> GMap -> MapSize -> TileSize -> Pos -> Int -> m ()
mapDraw re surfs gmap mSize tSize mPos count = do 
  let enums = repeat 0
      vGmap = visibleGmap gmap mSize mPos
  textures <- createTextures re surfs enums count 
  texMapDraw re textures vGmap tSize mapUpLeftPos 
  mapM_ destroyTexture textures

texMapDraw :: (MonadIO m) => Renderer -> [Texture] -> GMap -> TileSize -> Pos -> m ()
texMapDraw _ _ [] _ _ = return ()
texMapDraw re tx (ln:xs) tSize pos = do
  texLineDraw re tx ln tSize pos
  texMapDraw re tx xs tSize (pos + V2 0 tSize)

texLineDraw :: (MonadIO m) => Renderer -> [Texture] -> String -> TileSize -> Pos -> m ()
texLineDraw _ _ [] _ _ = return ()
texLineDraw re tx (t:ts) tSize pos = do
  texDraw re (tx!!read [t]) tSize pos
  texLineDraw re tx ts tSize (pos + V2 tSize 0)

texDraw :: (MonadIO m) => Renderer -> Texture -> TileSize -> Pos -> m ()
texDraw re tx tSize pos = copy re tx (Just (Rectangle (P (V2 0 0)) (V2 64 64)))
                                     (Just (Rectangle (P pos) (V2 tSize tSize)))

createTextures :: (MonadIO m) => Renderer -> [Surface] -> [EffectNum] -> Int -> m [Texture]
createTextures _ [] [] _ = return []
createTextures _ _ [] _ = return []
createTextures _ [] _ _ = return []
createTextures re (s:srs) (e:es) count = do 
  tx <- createTexture re s e count
  textures <- createTextures re srs es count 
  return (tx:textures) 

createTexture :: (MonadIO m) => Renderer -> Surface -> EffectNum -> Int -> m Texture
createTexture re surf i count = do
  if i==0 then createTextureFromSurface re surf
          else do 
            nsurf <- liftIO$createNewSurface surf count
            createTextureFromSurface re nsurf 

createNewSurface :: Surface -> Int -> IO Surface
createNewSurface surf count = do
  lockSurface surf
  SurfacePixelFormat pointerPixFormat <- surfaceFormat surf
  surPixFormat <- peek pointerPixFormat
  let sPixFormat = fromNumber (SDLT.pixelFormatFormat surPixFormat) :: PixelFormat
  pointer <- surfacePixels surf
  frPointer <- newForeignPtr_ (castPtr pointer)
  let mvector = VM.unsafeFromForeignPtr0 frPointer (4*64*64)
  unlockSurface surf
  waveEffect mvector count
  createRGBSurfaceFrom mvector (V2 64 64) (4*64) sPixFormat

waveEffect :: VM.IOVector Word8 -> Int -> IO ()
waveEffect vect t = do
  let defs = cosList 64 1 2 t
  lst <- makeList vect 64 0 (V2 0 0)
  let sList = zipWith (shiftList (V4 255 0 0 0)) lst defs
  let tList = transpose sList
  let s2List = zipWith (shiftList (V4 255 0 0 0)) tList defs
  let fList = transpose s2List
  writeList vect fList 0

writeList :: VM.IOVector Word8 -> [[V4 Word8]] -> Int -> IO ()
writeList _ [] _ = return ()
writeList vect (y:ys) si = do
  writeListX vect y si
  writeList vect ys (si+4*64)

writeListX :: VM.IOVector Word8 -> [V4 Word8] -> Int -> IO ()
writeListX _ [] _ = return ()
writeListX vect ((V4 a b c d):xs) si = do
  mapM_ (uncurry (VM.write vect)) (zip (map (+si) [0,1,2,3]) [a,b,c,d])
  writeListX vect xs (si+4)

makeList :: VM.IOVector Word8 -> Int -> Int -> V2 Int -> IO [[V4 Word8]] 
makeList vect u si (V2 _ q) = do
  if q==u then return [] else do
    x <- makeListX vect u si (V2 0 q)
    xs <- makeList vect u si (V2 0 (q+1))
    return (x : xs)

makeListX :: VM.IOVector Word8 -> Int -> Int -> V2 Int -> IO [V4 Word8]
makeListX vect u si (V2 p q) = do
  if p==u then return [] else do
    x <- makeV4 vect (si+pToI (V2 p q))
    xs <- makeListX vect u si (V2 (p+1) q)
    return (x : xs)

makeV4 :: VM.IOVector Word8 -> Int -> IO (V4 Word8)
makeV4 vect i = do
  [a,b,c,d] <- mapM (VM.read vect) [i..(i+3)]
  return (V4 a b c d)

pToI :: V2 Int -> Int
pToI (V2 p q) = 4*(64*q + p)

