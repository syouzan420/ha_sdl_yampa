{-# LANGUAGE OverloadedStrings #-}
module MyLib (tpsForRelativeLine,locToIndex,breakText,toDotPos,addMidDots,selectNearest
             ,textToDots,textToJumps,dotsToText,jumpsToText,nextPos,textIns,lastTps,headTps
             ,takeCurrentLine,deleteCurrentLine,takeCodes) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Data.Maybe(fromMaybe)
import Data.List (nub,elemIndex)
import MyData (Pos,Dot,Jump,Mgn,Size,Attr(..),WMode(..),dotSize)

type Index = Int
type Line = Int
type Letter = Int
type Location = (Line,Letter)
type Rect = V4 CInt

codeSeparator :: T.Text
codeSeparator = "```"

takeCodes :: T.Text -> [T.Text]
takeCodes tx = takeCodes' False False (T.lines tx)

takeCodes' :: Bool -> Bool -> [T.Text] -> [T.Text]
takeCodes' _ _ [] = []
takeCodes' True True (x:xs)
  | x == codeSeparator = [T.empty]
  | otherwise = [x <> " " <> head (takeCodes' True True xs)]
takeCodes' True False (x:xs) 
  | x == codeSeparator = takeCodes' False False xs
  | otherwise = x : takeCodes' True False xs
takeCodes' False False (x:xs)
  | x == codeSeparator = takeCodes' True False xs
  | T.isPrefixOf codeSeparator x = [(T.drop (T.length codeSeparator) x <> " = ") <> head (takeCodes' True True xs)]
  | otherwise = takeCodes' False False xs

textIns :: T.Text -> Int -> T.Text -> T.Text
textIns tx tpsSt texSt = T.take tpsSt texSt <> tx <> T.drop tpsSt texSt 

lastTps :: Int -> T.Text -> Int
lastTps tpsSt texSt = let dropLines = T.lines$T.drop tpsSt texSt
                       in if null dropLines then tpsSt else tpsSt + T.length (head dropLines)

headTps :: Int -> T.Text -> Int
headTps tpsSt texSt = let takeLines = T.lines$T.take tpsSt texSt
                       in if null takeLines then tpsSt else tpsSt - T.length (last takeLines) - 1

takeCurrentLine :: Int -> T.Text -> T.Text
takeCurrentLine tpsSt texSt =
  let lTps = lastTps tpsSt texSt
   in last$T.lines$T.take lTps texSt

deleteCurrentLine :: Int -> T.Text -> T.Text
deleteCurrentLine tpsSt texSt =
  let lTps = lastTps tpsSt texSt
      takeText = T.take lTps texSt
      dropText = T.drop lTps texSt
      textForward 
          | takeText==T.empty = T.empty
          | T.last takeText =='\n' = T.init takeText 
          | otherwise = let iLine = init$T.lines takeText
                         in if null iLine then T.empty else T.unlines iLine
   in textForward <> if texSt == T.empty || dropText == T.empty then T.empty else T.tail dropText

tpsForRelativeLine :: WMode -> Size -> Mgn -> Attr -> Text -> Int -> Index -> Index 
tpsForRelativeLine wmdSt wszSt mgnSt atrSt texSt rdv ind =
  let (ln,lt) = indexToLoc wmdSt wszSt mgnSt atrSt texSt ind   
      nind = locToIndex wmdSt wszSt mgnSt atrSt texSt (ln+rdv,lt)
   in if nind<0 then ind else nind

indexToLoc :: WMode -> Size -> Mgn -> Attr -> Text -> Index -> Location
indexToLoc wmdSt wszSt mgnSt atrSt texSt ind = 
    indexToLoc' wmdSt wszSt mgnSt atrSt (T.take ind texSt) (0,0)

indexToLoc' :: WMode -> Size -> Mgn -> Attr -> Text -> Location -> Location
indexToLoc' wm ws mg at tx lc =
  let (ps,tw,nw) = (gps at,ltw at,lnw at)
   in case uncons tx of
    Nothing -> lc 
    Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch tw nw wm ps ws mg lc 
                     in indexToLoc' wm ws mg at{gps=npos} xs (nln,nlt)
--
locToIndex :: WMode -> Size -> Mgn -> Attr -> Text -> Location -> Index
locToIndex wmdSt wszSt mgnSt atrSt texSt tlc 
            = locToIndex' wmdSt wszSt mgnSt atrSt texSt tlc (0,0) 0 

locToIndex' :: WMode -> Size -> Mgn -> Attr -> Text 
                                -> Location -> Location -> Index -> Index
locToIndex' wm ws mg at tx tlc@(tln,tlt) lc@(ln,lt) ind
  | lc==tlc = ind 
  | ln>tln && tlt > lt = ind-1 
  | otherwise =
      case uncons tx of
        Nothing -> if tlt>lt || tln>ln then ind else (-1) 
        Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch tw nw wm ps ws mg lc 
                         in locToIndex' wm ws mg at{gps=npos} xs tlc (nln,nlt) (ind+1)
  where (ps,tw,nw) = (gps at,ltw at,lnw at)

nextPos :: Char -> CInt -> CInt -> WMode -> Pos -> Size -> Mgn -> Location 
                                                    -> ((Bool,Bool),(Pos,Location))
nextPos ch tw nw wm ps@(V2 ox oy) (V2 ww wh) (V4 mr mt ml mb) (ln,lt) = 
    let cn = fromEnum ch
        htw = tw `div` 2
        ihf = cn > 31 && cn < 127
        irt = ch `T.elem` "＝ー「」（）：；"
        inl = ch == '\n'
        ins = ch `T.elem` "\n"
        delta 
           | wm==T = if ihf then V2 0 htw else V2 0 tw
           | ihf = V2 htw 0 
           | otherwise = V2 tw 0
        psd@(V2 nx ny) = if ins then ps else ps + delta 
        npos
           | wm==T = if ny > wh - mb || inl then V2 ox mt - V2 nw 0 else psd 
           | nx  > ww - mr || inl = V2 ml oy + V2 0 nw
           | otherwise = psd
        (nln,nlt)
           | wm==T && (ny > wh - mb || inl) = (ln+1,0)
           | wm==Y && (nx > ww - mr || inl) = (ln+1,0)
           | otherwise = (ln,lt+1)
     in ((ihf,irt),(npos,(nln,nlt)))


breakText :: Text -> (Text,Text)
breakText tx = let (hd,tl) = T.break (=='\n') tx
                   (hda,hdb) = if ' ' `T.elem` hd then T.break (==' ') hd else (hd,T.empty)
                   tl2
                     |tl==T.empty = tl 
                     |T.head tl == '\n' = " "<>tl
                     |otherwise = tl
                in (hda,hdb<>tl2)

dotsToRect :: [Dot] -> [Rect]
dotsToRect dtl = let ds = dotSize
                 in map (\(V2 x y,_) -> V4 (x*ds) (y*ds) ds ds) dtl 

toDotPos :: Pos -> Pos -> Pos
toDotPos (V2 px py) (V2 sx sy) = let ds = dotSize
                                     nx = (px-sx) `div` ds; ny = (py-sy) `div` ds
                                  in V2 nx ny

addMidDots :: Dot -> Dot -> [Dot] 
addMidDots (V2 x0 y0,cn) (V2 x1 y1,_) 
  | x0==x1 && y0==y1 = []
  | x0==x1 = map (\doty -> (V2 x0 doty,cn)) (if y1>y0 then [y0..y1] else [y1..y0])
  | y0==y1 = map (\dotx -> (V2 dotx y0,cn)) (if x1>x0 then [x0..x1] else [x1..x0])
  | otherwise = nub$map (\dotx -> (V2 dotx (f dotx),cn)) (if x1>x0 then [x0..x1] else [x1..x0]) ++
                    map (\doty -> (V2 (g doty) doty,cn)) (if y1>y0 then [y0..y1] else [y1..y0])
     where f x = floor$(fromIntegral (y1-y0) ::Double)/fromIntegral (x1-x0)*(fromIntegral x-fromIntegral x0) + fromIntegral y0
           g y = floor$(fromIntegral (x1-x0) ::Double)/fromIntegral (y1-y0)*(fromIntegral y-fromIntegral y0) + fromIntegral x0

selectNearest :: Int -> [Int] -> Int
selectNearest i ts = let devs = map (\t -> abs (i-t)) ts  
                      in fromMaybe 0 $ elemIndex (minimum devs) devs

textToDots :: [T.Text] -> [Dot]
textToDots [] = []
textToDots [_] = []
textToDots [_,_] = []
textToDots (x:y:c:xs) = (V2 (read$T.unpack x) (read$T.unpack y),read$T.unpack c):textToDots xs

textToJumps :: [T.Text] -> [Jump]
textToJumps [] = []
textToJumps [_] = []
textToJumps [_,_] = []
textToJumps [_,_,_] = []
textToJumps (fi:fnm:tgp:tgn:xs) = ((read$T.unpack fi,fnm),(read$T.unpack tgp,tgn)):textToJumps xs

dotsToText :: [Dot] -> T.Text
dotsToText dots = T.unwords$foldl (\acc (V2 x y,c) -> acc++[T.pack$show x,T.pack$show y,T.pack$show c]) [] dots

jumpsToText :: [Jump] -> T.Text
jumpsToText jmps = T.unwords$foldl (\acc ((fln,fnm),(tgp,tgn)) -> acc++[T.pack$show fln,fnm,T.pack$show tgp,tgn]) [] jmps

