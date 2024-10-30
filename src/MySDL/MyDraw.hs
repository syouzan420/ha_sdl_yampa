{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyDraw (myDraw,initDraw,textsDraw) where

import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..),textureAlphaMod
                          ,present,createTextureFromSurface,freeSurface,destroyTexture
                          ,fillRect,drawPoint)
import SDL (($=))
import SDL.Vect (Point(P),V2(..))
import SDL.Font (Font,blended)
import SDL.Primitive (thickLine,rectangle,circle,fillCircle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (foldM_,when,unless)
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Data.Text (pack)
import General (getIndex)
import MyData (State(..),Active(..),Attr(..),Jumping(..),WMode(..),FMode(..)
              ,IsFormat,Dot,Pos,TextPos,TextData
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..),Color
              ,fontSize,cursorColor,backColor,initTatePos,initYokoPos
              ,dotSize,colorPallet,statusPos,imageNames)

type IsCursor = Bool

myDraw :: (MonadIO m) => Renderer -> [Font] -> [Texture] 
                              -> TextData -> Bool -> State -> m () 
myDraw re fonts itex textData isOnlyMouse st = do
  let ac = act st 
      (dtsSt,drwSt,imgSt,atrSt,tpsSt,wmdSt,ifmSt,icrSt) =
        (dts ac,drw st,img st,atr st,tps ac,wmd st,ifm st,icr ac)
      scrAt = scr atrSt
      iniPos = if wmdSt==T then initTatePos else initYokoPos
  initDraw re
  statusDraw re (fonts!!1) st 
  unless isOnlyMouse $ textsDraw re fonts wmdSt ifmSt icrSt tpsSt textData
  when (tpsSt==0 && icrSt && not ifmSt) $ cursorDraw re (iniPos+scrAt) wmdSt (fromIntegral fontSize) 
  dotsDraw re scrAt dtsSt
  myDrawing re drwSt
  imageDraw re itex imgSt
  present re

imageDraw :: (MonadIO m) => Renderer -> [Texture] -> [Img] -> m ()
imageDraw re itex = mapM_ (\(Img pos siz rot name) -> 
  when (name `elem` imageNames) $ do
            let ind = getIndex name imageNames
            copyEx re (itex!!ind) (Just (Rectangle (P (V2 0 0)) (V2 64 64)))
                                  (Just (Rectangle (P pos) siz))
                                  (fromIntegral rot) Nothing (V2 False False) ) 

myDrawing :: (MonadIO m) => Renderer -> [Drw] -> m ()
myDrawing _ [] = return () 
myDrawing re ((Drw cn siz shp):drs) = do
  let col = colorPallet!!cn
  rendererDrawColor re $= col 
  drawShape re col siz shp
  myDrawing re drs

drawShape :: (MonadIO m) => Renderer -> Color -> CInt -> Shp -> m ()
drawShape re col siz (L (Li ps0 ps1)) = thickLine re ps0 ps1 siz col 
drawShape re col siz (R (Rc False (V2 x y) (V2 w h))) = 
  if w>siz && h>siz then mapM_ (\dp -> rectangle re (V2 (x+dp) (y+dp)) (V2 (x+w-dp) (y+h-dp)) col) [0..(siz-1)] 
                    else fillRect re (Just (Rectangle (P (V2 x y)) (V2 w h)))
drawShape re _ _ (R (Rc True ps wh)) = fillRect re (Just (Rectangle (P ps) wh)) 
drawShape re col siz (C (Cr False ps rd)) = 
  if rd>siz then mapM_ (\dp -> circle re ps (rd-dp) col) [0..(siz-1)] else fillCircle re ps rd col
drawShape re col _ (C (Cr True ps rd)) = fillCircle re ps rd col
drawShape re col siz (D (Dt ps)) =
  if siz==1 then drawPoint re (P ps) else fillCircle re ps (siz-1) col
--drawShape _ _ _ _ = return ()

dotsDraw :: (MonadIO m) => Renderer -> Pos -> [Dot] -> m () 
dotsDraw re (V2 sx sy) = mapM_ (\(V2 x y,cn) -> do
  let ds = dotSize
  rendererDrawColor re $= colorPallet!!cn 
  fillRect re (Just (Rectangle (P (V2 (x*ds+sx) (y*ds+sy))) (V2 ds ds)))
                    ) 

cursorDraw :: (MonadIO m) => Renderer -> Pos -> WMode -> CInt -> m () 
cursorDraw re (V2 x y) wm sz = do
  let rect = if wm==T then Rectangle (P (V2 x y)) (V2 sz 2) 
                      else Rectangle (P (V2 (x-1) y)) (V2 2 sz)
  rendererDrawColor re $= cursorColor 
  fillRect re (Just rect) 

statusDraw :: (MonadIO m) => Renderer -> Font -> State -> m ()
statusDraw re font st = do
  let fileNum = pack$show$fps$act st
      textPos = pack$show$tps$act st
      editMode = pack$show$emd st
      scroll = pack$show$scr (atr st)
      fromJump = pack$show$fjp$jmp (atr st)
      statusText = "fNum:"<>fileNum<>" tPos:"<>textPos<>" eMode:"<>editMode <>" scr:"<>scroll
                  <>" fjp:"<>fromJump
      ofs = fromIntegral fontSize
      lng = fromIntegral$T.length statusText
  fontS <- blended font (colorPallet!!1) statusText 
  fontT <- createTextureFromSurface re fontS
  mapM_ (\i -> do
     copy re fontT (Just (Rectangle (P (V2 (ofs*i) 0)) (V2 ofs ofs)))
                   (Just (Rectangle (P (statusPos+V2 (12*i) 0)) (V2 12 12)))
         ) [0::CInt,1..lng] 
  destroyTexture fontT
  freeSurface fontS
  

textsDraw :: (MonadIO m) => Renderer -> [Font] -> WMode 
        -> IsFormat -> IsCursor -> TextPos -> TextData -> m () 
textsDraw _ _ _ _ _ _ [] = return () 
textsDraw re fonts wmdSt ifmSt icrSt tpsSt ((iCur,tx,nat,pList):xs) = do
  let (scrAt,fszAt,fcoAt,fmdAt) = (scr nat,fsz nat,fco nat,fmd nat)
      ofs = fromIntegral fontSize
      fs = fromIntegral fszAt
      fnum = case fmdAt of Min -> 0; Got -> 1; Ost -> 2
      nscr = if null xs then scrAt else let (_,_,nxtAtr,_) = head xs in scr nxtAtr
      rpText = T.replace "\n" "  " tx
      rpText2 = T.replace "\n" "　" tx

      lPos = snd$last pList
      tx' = if fnum==0 then T.pack $ fst $ unzip $ filter (\(_,((b,_),_)) -> not b) (zip (T.unpack rpText2) pList) else tx
      pList' = if fnum==0 then filter (\((b,_),_)->not b) pList else pList
  when (tx'/=T.empty) $ do
        fontS <- case fnum of
                 0 -> blended (fonts!!fnum) fcoAt tx' 
                 1 -> blended (fonts!!fnum) fcoAt tx 
                 2 -> blended (fonts!!fnum) fcoAt rpText
                 _ -> blended (fonts!!1) fcoAt tx
        fontT <- createTextureFromSurface re fontS
        foldM_ (\ ps ((b,r),pd) -> do
          let sz = if b then ofs `div` 2 else ofs
          copyEx re fontT (Just (Rectangle (P ps) (V2 sz ofs)))
                          (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
                          (if wmdSt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 sz 0)
              ) (V2 0 0) pList'
        destroyTexture fontT
        freeSurface fontS
  when (tx/=T.empty && fnum==0) $ do
        fontS2 <- blended (fonts!!1) fcoAt tx
        fontT2 <- createTextureFromSurface re fontS2
        foldM_ (\ ps ((b,r),pd) -> do
          let sz = if b then ofs `div` 2 else ofs
          when b $ do
            copyEx re fontT2 (Just (Rectangle (P ps) (V2 sz ofs)))
                             (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
                             (if wmdSt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 sz 0)
              ) (V2 0 0) pList
        destroyTexture fontT2
        freeSurface fontS2
  when (iCur && icrSt && not ifmSt) $ cursorDraw re (lPos+nscr) wmdSt fs 
  textsDraw re fonts wmdSt ifmSt icrSt tpsSt xs

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re


