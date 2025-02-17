{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyDraw (myDraw,initDraw,textsDraw) where

import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..)
    ,present,createTextureFromSurface,freeSurface,destroyTexture
    ,fillRect,drawPoint)
import SDL (($=))
import SDL.Vect (Point(P),V2(..))
import SDL.Font (Font,blended,size)
import SDL.Primitive (thickLine,rectangle,circle,fillCircle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (foldM_,when,unless)
import Data.Bifunctor (first)
--import Data.List (partition)
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Data.Text (pack)
import General (getIndex)
import MyAction (getPicIndex)
import MyData (State(..),Active(..),Attr(..),Jumping(..),WMode(..),FMode(..)
              ,IsFormat,Dot,Pos,TextPos,TextData(..),ChPos(..),PointSize,Size
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..),Color
              ,fontSize,cursorColor,backColor,initTatePos,initYokoPos
              ,dotSize,colorPallet,statusPos,imageNames)

type IsCursor = Bool

myDraw :: (MonadIO m) => Renderer -> [Font] -> [Texture] 
                              -> [TextData] -> Bool -> State -> m () 
myDraw re fonts itex textDatas isOnlyMouse st = do
  let ac = act st 
      (dtsSt,drwSt,imgSt,iszSt,atrSt,tpsSt,wmdSt,ifmSt,icrSt,iblSt) =
        (dts ac,drw st,img st,isz st,atr st,tps ac,wmd st,ifm st,icr ac,ibl st)
      scrAt = scr atrSt
      iniPos = if wmdSt==T then initTatePos else initYokoPos
  initDraw re
  statusDraw re (fonts!!1) st 
  unless (isOnlyMouse || iblSt) $
       textsDraw re fonts itex fontSize wmdSt ifmSt icrSt iszSt tpsSt textDatas
  when (tpsSt==0 && icrSt && not ifmSt) $ cursorDraw re (iniPos+scrAt) wmdSt (fromIntegral fontSize) 
  dotsDraw re scrAt dtsSt
  myDrawing re drwSt
  imageDraw re itex iszSt imgSt
  present re

imageDraw :: (MonadIO m) => Renderer -> [Texture] -> [Size] -> [Img] -> m ()
imageDraw re itex iszSt = mapM_ (\(Img pos siz rot name) -> 
  when (name `elem` imageNames) $ do
            let ind = getIndex name imageNames
                dSize = iszSt!!ind
            copyEx re (itex!!ind) (Just (Rectangle (P (V2 0 0)) dSize))
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

dotsDraw :: (MonadIO m) => Renderer -> Pos -> [Dot] -> m () 
dotsDraw re (V2 sx sy) dots = do 
  mapM_ (\(V2 x y,cn) -> do
    let ds = dotSize
    rendererDrawColor re $= colorPallet!!cn 
    fillRect re (Just (Rectangle (P (V2 (x*ds+sx) (y*ds+sy))) (V2 ds ds)))
                               ) dots 
--  diaDraw re (V2 sx sy) dots

{--
diaDraw :: (MonadIO m) => Renderer -> Pos -> [Dot] -> m ()
diaDraw _ _ [] = return ()
diaDraw re s ((p,_):ds) = do diagonalDraw re s p ds
                             diaDraw re s ds

findTriangle :: Pos -> Pos -> Maybe ((Pos,Pos,Pos),(Pos,Pos,Pos))
findTriangle (V2 x y) (V2 x1 y1)
  | x1 == x+1 && y1 == y-1 = Just ((V2 px py,V2 tx py,V2 tx ty)
                                  ,(V2 tx py,V2 tx (py+ds),V2 (tx+ds) py))
  | x1 == x-1 && y1 == y-1 = Just ((V2 px ty,V2 px py,V2 (px+ds) py)
                                  ,(V2 tx py,V2 px (py+ds),V2 px py))
  | x1 == x-1 && y1 == y+1 = Just ((V2 px py,V2 tx ty,V2 px ty)
                                  ,(V2 px ty,V2 px (ty+ds),V2 (px+ds) ty))
  | x1 == x+1 && y1 == y+1 = Just ((V2 px ty,V2 tx (ty+ds),V2 tx ty)
                                  ,(V2 tx py,V2 tx ty,V2 (tx+ds) ty))
  | otherwise = Nothing
  where ds = dotSize
        px = x*ds; py = y*ds; tx = x1*ds; ty = y1*ds

diagonalDraw :: (MonadIO m) => Renderer -> Pos -> Pos -> [Dot] -> m ()
diagonalDraw _ _ _ [] = return ()
diagonalDraw re s pos0 ((pos1,cn):xs) = do
  let tri = findTriangle pos0 pos1
  let col = colorPallet!!cn
  let nextCheck = diagonalDraw re s pos0 xs
  case tri of
    Just ((p0,p1,p2),(p3,p4,p5)) -> do fillTriangle re (p0+s) (p1+s) (p2+s) col
                                       fillTriangle re (p3+s) (p4+s) (p5+s) col
                                       nextCheck
    Nothing -> nextCheck
--}

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
      command = pack$com st
      statusText = "fNum:"<>fileNum<>" tPos:"<>textPos<>" eMode:"<>editMode <>" scr:"<>scroll
                  <>" fjp:"<>fromJump<>" com:"<>command
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
  

textsDraw :: (MonadIO m) => Renderer -> [Font] -> [Texture] -> PointSize -> WMode 
        -> IsFormat -> IsCursor -> [Size] -> TextPos -> [TextData] -> m () 
textsDraw _ _ _ _ _ _ _ _ _ [] = return () 
textsDraw re fonts itex dfsz wmdSt ifmSt icrSt iszSt tpsSt (TD iCur tx nat pList:xs)
  = do
  let (scrAt,fszAt,fcoAt,fmdAt) = (scr nat,fsz nat,fco nat,fmd nat)
      ofs = fromIntegral dfsz 
      fs = fromIntegral fszAt
      fnum = case fmdAt of Min -> 0; Got -> 1; Ost -> 2; Azu -> 3;
      nscr = if null xs then scrAt else let (TD _ _ nxtAtr _) = head xs in scr nxtAtr
      rpText = T.replace "\n" "  " tx
      rpText2 = T.replace "\t" "　" $ 
                T.replace "\r" "　" $
                T.replace "\n" "　" tx

      CP _ _ lPos = last pList
--      (txPListHalf,txPListWhole) =
--           partition (\(_,((b,_),_)) -> b) (zip (T.unpack rpText2) pList)
--      (tx', pListWhole) = first T.pack $ unzip txPListWhole
      (tx',pListWhole) = if fnum==0 then first T.pack $ unzip $
              filter (\(_,CP b _ _) -> not b) (zip (T.unpack rpText2) pList)
                                else (tx, pList)
      fText = case fnum of 0 -> tx'; 1 -> tx; 2 -> rpText; 3 -> rpText2; _ -> tx;
      fnum' = if fnum > 3 then 1 else fnum
      iShowPic = ifmSt && (T.take 4 tx' == "pic ")
  when iShowPic $ do 
        let iname = T.drop 4 tx'
        let ind = getPicIndex iname
        let (CP _ _ ipos) = head pList
        when (ind>=0) $ 
          imageDraw re itex iszSt
               [Img (ipos+nscr) (iszSt!!ind) 0 (T.unpack iname)]   
  when (tx'/=T.empty && not iShowPic) $ do
        fontS <- blended (fonts!!fnum') fcoAt fText 
        fontT <- createTextureFromSurface re fontS
        foldM_ (\ ps (CP b r pd) -> do
          let sz = if b then ofs `div` 2 else ofs
          copyEx re fontT 
            (Just (Rectangle (P ps) (V2 sz ofs)))
            (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
            (if wmdSt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 sz 0)
              ) (V2 0 0) pListWhole
        destroyTexture fontT
        freeSurface fontS
  when (tx/=T.empty && fnum==0 && not iShowPic) $ do
        fontS2 <- blended (fonts!!4) fcoAt tx
        (sz,szh) <- size (fonts!!4) "a"
        let (fszX,fszY) = (fromIntegral sz, fromIntegral szh)
        fontT2 <- createTextureFromSurface re fontS2
        foldM_ (\ ps (CP b r pd) -> do
 --         let sz = if b then ofs `div` 2 else ofs
          when b $ do
            copyEx re fontT2 
             (Just (Rectangle (P ps) (V2 fszX fszY)))
             (Just (Rectangle (P (pd+nscr)) (V2 fszX fszY)))
--             (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
             (if wmdSt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 fszX 0)
              ) (V2 0 0) pList
        destroyTexture fontT2
        freeSurface fontS2
  when (iCur && icrSt && not ifmSt) $ cursorDraw re (lPos+nscr) wmdSt fs 
  textsDraw re fonts itex dfsz wmdSt ifmSt icrSt iszSt tpsSt xs

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re


