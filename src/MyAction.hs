{-# LANGUAGE OverloadedStrings #-}
module MyAction (myAction,beforeDraw,afterDraw,makePList,changeAtr
                ,exeAttrCom,makeTextData,makeTexts,getCid) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import SDL.Vect (V2(..),V4(..))
import Data.Maybe(fromMaybe)
import Data.List(elemIndex)
import MyLib (breakText,nextPos)
import MyData (IsFormat,TextPos,TextData,Jump,FrJp,Mgn,Size
              ,State(..),Active(..),Attr(..),Rubi(..),Jumping(..),WMode(..)
              ,rubiSize,textLengthLimit,linkColor,selectColor,fontColor,cursorTime)

type Index = Int
type FilePos = Int
type IsDialog = Bool

myAction :: State -> State
myAction st = st

beforeDraw :: State -> State 
beforeDraw st = 
  let crcSt = (crc.act) st
      icrSt = (icr.act) st
      ncrc = if crcSt<cursorTime then crcSt+1 else 0
      nicr = if crcSt<cursorTime then icrSt else not icrSt
   in st{act=(act st){crc=ncrc, icr=nicr}}

afterDraw :: State -> State
afterDraw st = st

makeTextData :: State -> TextData 
makeTextData st =
  let ac = act st 
      (texSt,etxSt,fpsSt,tpsSt,wszSt,mgnSt,atrSt,wmdSt,ifmSt) 
              = (tex ac,etx ac,fps ac,tps ac,wsz st,mgn st,atr st,wmd st,ifm st)
   in makeTexts 0 ifmSt wmdSt fpsSt tpsSt wszSt mgnSt atrSt etxSt texSt

makeTexts :: Index -> IsFormat -> WMode -> FilePos -> TextPos ->
                              Size -> Mgn -> Attr -> Text -> Text -> TextData 
makeTexts ind ifmSt wmdSt fpsSt tpsSt wszSt mgnSt atrSt etxSt texSt = 
  case uncons texSt of
    Nothing -> if texSt=="" && tpsSt==0 
                  then [(True,etxSt,atrSt,makePList wmdSt wszSt mgnSt atrSt etxSt)] else []
    Just (ch,tailTx) ->  
      let (natr,(ptx,pxs)) 
            | ifmSt = if ch==';' then exeAttrCom wmdSt fpsSt ind (changeAtr atrSt{ite=False} tailTx) 
                 else if cnm atrSt/=T.empty then exeAttrCom wmdSt fpsSt ind (atrSt{ite=False},texSt)
                                            else (atrSt,T.break (==';') texSt)
            | otherwise = (atrSt,(texSt,T.empty))
          tll = textLengthLimit
          (ptx2,pxs2) = if T.length ptx>tll then (T.take tll ptx,T.drop tll ptx<>pxs) else (ptx,pxs)
          lnTex = T.length texSt 
          preInc = lnTex - T.length pxs2 + 1
          iCur = tpsSt > ind && tpsSt < ind + preInc
          (iptx,tptx) = if iCur && tpsSt>0 then T.splitAt (tpsSt-ind) ptx2 else (ptx2,T.empty) 
          (tx,xs) = if iCur then (iptx<>etxSt,tptx<>pxs2) else (ptx2,pxs2)
          (scrAt,fszAt) = (scr natr,fsz natr)
          fs = fromIntegral fszAt
          pList = makePList wmdSt wszSt mgnSt natr tx
          indInc = lnTex - T.length xs 
          lPos@(V2 lpx lpy) = snd$last pList
          (V2 sx sy) = scrAt
          (V2 ww wh) = wszSt
          (V4 mr mt ml mb) = mgnSt
          nscr
            | iCur && wmdSt == T && lpx+sx < ml = V2 (ml-lpx) sy 
            | iCur && wmdSt == T && (cnm natr)/="rb" && lpx+sx > ww - mr - fs  
                                              = V2 (ww-mr-fs*2-lpx) sy
            | iCur && wmdSt == Y && lpy+sy > wh - mb - fs = V2 sx (wh-mb-fs-lpy)
            | iCur && wmdSt == Y && lpy+sy < mt = V2 sx (mt+fs-lpy)
            | otherwise = scrAt
      in (iCur,tx,natr{gps=lPos,scr=nscr},pList):makeTexts (ind+indInc) ifmSt wmdSt fpsSt tpsSt wszSt mgnSt natr{gps=lPos,scr=nscr} etxSt xs 

makePList :: WMode -> Size -> Mgn -> Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList wm ws mg at tx = 
  let (ps@(V2 ox oy),tw,nw) = (gps at,ltw at,lnw at)
   in case uncons tx of
    Nothing -> [((False,False),ps)]
    Just (ch,xs) -> let ((ihf,irt),(npos,_)) = nextPos ch tw nw wm ps ws mg (0,0) 
                        qtw = tw `div` 4
                        ihft = wm==T && ihf
                     in ((ihf,irt),V2 (if ihft then ox+qtw else ox) (if ihft then oy-qtw else oy))
                          :makePList wm ws mg at{gps=npos} xs

changeAtr :: Attr -> Text -> (Attr, Text)
changeAtr attr tx = 
  let (ncid, (cm, rtx)) = getCid tx
      natr = attr{cnm=cm, cid=ncid}
   in (natr , rtx)

getCid :: Text -> (Int, (Text,Text))
getCid tx =
  let (cm,rtx) = T.break (==' ') tx
      ncid = case cm of
               "rb" -> 2 
               "jtg" -> 1
               "jp" -> 3
               _    -> 0
   in (ncid, (cm, rtx))


exeAttrCom :: WMode -> FilePos -> TextPos -> (Attr,Text) -> (Attr, (Text, Text))
exeAttrCom wmdSt fpsSt tpsSt (at,tx) = 
  let jmpAt = jmp at 
      (gpsAt,fszAt,ltwAt,rbiAt,dtaAt,jpsAt,fjpAt,sjnAt,cnmAt,cidAt) =
        (gps at,fsz at,ltw at,rbi at
        ,dta jmpAt,jps jmpAt,fjp jmpAt,sjn jmpAt,cnm at,cid at) 
      (Rubi rpsRb rwdRb tszRb tlwRb sprRb) = rbiAt
      tailTx = T.tail tx
      (ttx,rtx) = if cidAt>0 then breakText tailTx  else T.break (==';') tailTx
      tln = fromIntegral (T.length ttx)
      natr = case cnmAt of
               "rb" -> case cidAt of
                         2 -> at{rbi=rbiAt{rps=gpsAt,rwd=ltwAt*tln}}
                         1 -> let fs = fromIntegral fszAt
                                  rbStartPos = if wmdSt==T then rpsRb + V2 (fs+sprRb) 0  
                                                           else rpsRb - V2 0 (fromIntegral rubiSize+sprRb)
                                  rbLetterWidth = rwdRb `div` tln 
                               in at{gps=rbStartPos,fsz=rubiSize,ltw=rbLetterWidth 
                                    ,rbi=rbiAt{tsz=fszAt,tlw=ltwAt}} 
                         0 -> at{gps=rpsRb+(if wmdSt==T then V2 0 rwdRb else V2 rwdRb 0)
                                  ,fsz=tszRb, ltw=tlwRb}
                         _ -> at
               "jtg" -> case cidAt of
                          1 -> let jd = textToJumpData fpsSt tpsSt ttx
                                   dataExist = jd `elem` jpsAt
                                   njps = if dataExist then jpsAt else jpsAt ++ [jd]
                                in at{jmp=(jmp at){jps=njps},ite=True}
                          _ -> at
               "jp" -> case cidAt of
                         3 -> at{jmp=(jmp at){dta=[ttx]},ite=True} 
                         2 -> at{jmp=(jmp at){dta=dtaAt++[ttx]},ite=True} 
                         1 -> let tjp = searchJump jpsAt dtaAt tpsSt 
                                  dataExist = tjp `elem` fjpAt
                                  nfjp 
                                    |fst tjp==(-1) = fjpAt
                                    |dataExist = fjpAt
                                    |otherwise = fjpAt ++ [tjp]
                                  ind = fromMaybe (-1) $ elemIndex tjp nfjp
                                  nfco
                                    |sjnAt==ind = selectColor
                                    |fst tjp/=(-1) = linkColor
                                    |otherwise = fontColor
                               in at{jmp=(jmp at){fjp=nfjp},fco=nfco}
                         0 -> at{fco=fontColor} 
                         _ -> at
               _    -> at{cnm=""}
      ncnm = if cidAt==0 then "" else cnmAt
   in (natr{cnm=ncnm, cid=cidAt-1} , (if ite natr then "" else ttx, rtx))

textToJumpData :: FilePos -> TextPos -> Text -> Jump
textToJumpData fpsSt tpsSt ttx = ((fpsSt,T.pack$show fpsSt),(tpsSt,ttx)) 

searchJump :: [Jump] -> [Text] -> TextPos -> FrJp 
searchJump [] _ _ = (-1,(0,0))
searchJump (((fi,fnm),(tp,tn)):xs) dtaAt tpsSt
  | length dtaAt/=2 = (-1,(0,0)) 
  | fnm==head dtaAt && tn==last dtaAt = (tpsSt,(fi,tp))
  | otherwise = searchJump xs dtaAt tpsSt

