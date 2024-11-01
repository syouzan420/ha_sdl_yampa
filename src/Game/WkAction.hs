{-# LANGUAGE OverloadedStrings #-}
module Game.WkAction (wkInput,initInput,startText,makeWkTextData) where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import qualified Data.Text as T
import SDL.Input.Keyboard.Codes
import MySDL.MyInput (myInput,InpRes(..))
import Data.Maybe (fromMaybe)
import FRP.Yampa (DTime)
import qualified MyData as MD 
import MyAction (makeTexts)
import Game.WkData (Waka(..),Input(..),windowSize)


initInput :: MonadIO m => S.StateT Waka m Input
initInput = return No

--wkInput :: (MonadIO m) => m Input
wkInput :: MonadIO m => Bool -> S.StateT Waka m (DTime, Maybe Input)
wkInput _ = do
    InpRes kc _ _ _ _ _ ir <- myInput
    let inp = if ir then Rl else case kc of
          KeycodeEscape -> Es
          KeycodeSpace -> Sp
          KeycodeK -> Up
          KeycodeUp -> Up
          KeycodeJ -> Dn
          KeycodeDown -> Dn
          KeycodeH -> Lf
          KeycodeLeft -> Lf
          KeycodeL -> Ri
          KeycodeRight -> Ri
          KeycodeReturn -> Rt
          _else -> No
    return (1,Just inp)

startText :: Text -> Text -> Waka -> Waka 
startText sIndex allText wk = do
  let lns = T.lines allText 
      tset = makeIndex lns
      iText = if null tset then allText 
                           else fromMaybe (snd (head tset)) (lookup sIndex tset)
   in wk{set=tset,tex=iText}

makeIndex :: [Text] -> [(Text,Text)]  
makeIndex [] = []
makeIndex (tx:txs) =
 let (ind,ch) = fromMaybe (T.empty,'0') (T.unsnoc tx) 
  in if ch==':' then let (text,xs) = getText txs [] in (ind,text):makeIndex xs 
                else makeIndex txs

getText :: [Text] -> [Text] -> (Text,[Text])
getText [] acc = (T.unlines acc, [])
getText (tx:txs) acc =
  let (_,ch) = fromMaybe (T.empty,'0') (T.unsnoc tx)
   in if ch==':' then (T.unlines acc,tx:txs) 
                 else getText txs (acc++[tx]) 

makeWkTextData :: Waka -> MD.TextData
makeWkTextData wk =
  let (stxWk,scrWk,tmdWk,rctWk,mgnWk,ltwWk,lnwWk,fszWk) 
        = (stx wk,scr wk,tmd wk,rct wk,mgn wk,ltw wk,lnw wk,fsz wk)
      (V2 ww wh) = windowSize
      (V4 x y w h) = rctWk 
      (V4 rm tm lm bm) = mgnWk
      sTps = T.length stxWk - 1
      normalMgn = V4 (ww-(x+w)+rm) (y+tm) (x+lm) (wh-(y+h)+bm)
      mgn' = case tmdWk of
              0 -> V4 (div ww 2-20) (tm+60) lm bm
              _ -> normalMgn 
      (V4 rm' tm' _ _) = mgn'
      initPos = V2 (ww-rm'-fromIntegral fszWk) tm'
      atr' = MD.initAttr{MD.gps=initPos+scrWk,MD.scr=scrWk, MD.ltw=ltwWk
               ,MD.lnw=lnwWk,MD.fsz=fszWk}
   in makeTexts 0 True MD.T 0 sTps windowSize mgn' atr' T.empty stxWk 

