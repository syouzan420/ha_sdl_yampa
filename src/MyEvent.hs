{-# LANGUAGE OverloadedStrings #-}
module MyEvent (inputEvent,initInput) where

import MySDL.MyInput (myInput,InpRes(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import qualified Control.Monad.State.Strict as S
import Control.Monad (when)
import qualified Data.Text as T
import MyData (State(..),Active(..),Attr(..),Coding(..),Jumping(..),Modif(..),WMode(..),EMode(..),FMode(..),Input(..),initYokoPos,initTatePos,colorPallet)
import MyLib (tpsForRelativeLine,locToIndex,toDotPos,addMidDots,selectNearest,textIns,lastTps,takeCurrentLine,deleteCurrentLine,headTps,toDig)
import Mana.Mana (evalCode,taiyouMn,Mn(..),Yo(..),Dtype(..),preDef,userDef)
import SDL.Raw.Types (Rect(..))
import SDL.Input.Keyboard (startTextInput,stopTextInput)
import SDL.Input.Keyboard.Codes
import FRP.Yampa (DTime)

initInput :: S.StateT State IO (Input,Bool) 
initInput = return (NON,False)

inputEvent :: Bool -> S.StateT State IO (DTime, Maybe (Input,Bool))
inputEvent _ = do
  evInput <- myInput
  case evInput of
    Nothing -> return (1,Just (NON,False))
-- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
    Just (InpRes kc md it mps isc ised _) -> do
      st <- S.get
      let (actSt,cdnSt) = (act st, cdn st) 
          (texSt,etxSt,dtsSt,tpsSt,dfnSt,digSt,comSt,wszSt,mgnSt,atrSt,emdSt,wmdSt,cplSt,ifmSt,iskSt)
            = (tex actSt,etx actSt,dts actSt,tps actSt,dfn cdnSt
              ,dig st,com st,wsz st,mgn st,atr st,emd st,wmd st,cpl st,ifm st,isk st)
          isKeyPressed = kc/=KeycodeUnknown
          isMousePressed = mps/=V2 (-1) (-1)
          isQuit = kc==KeycodeEscape   -- ESC Key

          isNor = emdSt==Nor
          isIns = emdSt==Ins
          isDig = emdSt==Dgr
          isRet = kc==KeycodeReturn

      ----normal mode

          isUp = (kc==KeycodeK && isNor) || kc==KeycodeUp
          isDown = (kc==KeycodeJ && isNor) || kc==KeycodeDown
          isLeft = (kc==KeycodeH && isNor) || kc==KeycodeLeft
          isRight = (kc==KeycodeL && isNor) || kc==KeycodeRight

          isToIns = kc==KeycodeI && isNor

      ----normal with shift key

          isFarForward = kc==KeycodeF && md==Shf && isNor
          isFarBack = kc==KeycodeB && md==Shf && isNor

      ----insert mode

          isToNor = isExit && isIns
          isToDig = kc==KeycodeK && md==Ctr && isIns

      ----with ctrl key

          isNewFile = kc==KeycodeN && md==Ctr
          isLoadFile = kc==KeycodeL && md==Ctr
          isLoadPrevFile = kc==KeycodeP && md==Ctr
          isLoadRecentFile = kc==KeycodeR && md==Ctr

          isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)

          isExit = kc==KeycodeLeftBracket && md==Ctr

          isTglOsd = kc==KeycodeO && md==Ctr
          isTglMin = kc==KeycodeM && md==Ctr
          isTglFmt = kc==KeycodeF && md==Ctr

          isFontPlus = kc==KeycodeEquals && md==Ctr
          isFontMinus = kc==KeycodeMinus && md==Ctr

          isExeCode = kc==KeycodeE && md==Ctr
          isExeDig = isDig && length comSt == 1 

          isDrawClear = kc==KeycodeD && md==Ctr
          isTglColor = kc==KeycodeC && md==Ctr


          isJump = ifmSt && isRet && (sjn.jmp) atrSt>=0
          isJBak = ifmSt && isBS

          isSkkEdit = it==T.empty && kc/=KeycodeRShift && kc/=KeycodeLShift 
                              && kc/=KeycodeUnknown && md==Shf

          isBS = (kc==KeycodeBackspace && not iskSt) || (isNor && kc==KeycodeX)
          isDeleteLine = not (null comSt) && last comSt=='d' && kc==KeycodeD 
          isToTop = not (null comSt) && last comSt=='g' && kc==KeycodeG

          isCom = md==Alt
          comName = case kc of
                  KeycodeR -> ";rb "
                  _other   -> T.empty

          (fm,lw,fjpAt,fszAt,ltwAt,lnwAt,V2 ww wh,V4 mr mt ml mb,scrAt@(V2 sx sy)) =
              (fmd atrSt,lnw atrSt,(fjp.jmp) atrSt
              ,fsz atrSt,ltw atrSt,lnw atrSt,wszSt,mgnSt,scr atrSt)
          tLen = T.length texSt
          seeLines = fromIntegral$if wmdSt==T then (ww-mr-ml) `div` lw else (wh-mt-mb) `div` lw
          tpsPreLine = tpsForRelativeLine wmdSt wszSt mgnSt atrSt texSt (-1) tpsSt
          tpsNextLine = tpsForRelativeLine wmdSt wszSt mgnSt atrSt texSt 1 tpsSt
          tpsFarBack = tpsForRelativeLine wmdSt wszSt mgnSt atrSt texSt (-seeLines) tpsSt
          tpsFarForward = tpsForRelativeLine wmdSt wszSt mgnSt atrSt texSt seeLines tpsSt

          codeMana 
            | isExeCode = evalCode (preDef++[(User,userDef++dfnSt)]) (takeCurrentLine tpsSt texSt)
            | otherwise = Mn "" Moz
          (ta,yo) = taiyouMn codeMana
          codeResult 
            | isExeCode && yo==Io = T.empty 
            | isExeCode = "\n"<>(T.pack.show) codeMana 
            | otherwise = T.empty
      
          nit = if isIns && isRet && it==T.empty then "\n" else it
          centerLineNum = 
              if wmdSt==T then (ww-mr-ml) `div` lw `div` 2  + sx `div` lw 
                          else (wh-mt-mb) `div` lw `div` 2 - sy `div` lw
          centerIndex = locToIndex wmdSt wszSt mgnSt atrSt texSt (fromIntegral centerLineNum,0)
          nsjn = selectNearest centerIndex (map fst fjpAt)
          nscr
            | isToTop = V2 0 0
            | ifmSt && wmdSt==T && isLeft = scrAt+V2 lw 0
            | ifmSt && wmdSt==T && isRight = scrAt-V2 lw 0
            | ifmSt && wmdSt==Y && isUp = scrAt+V2 0 lw
            | ifmSt && wmdSt==Y && isDown = scrAt-V2 0 lw
            | otherwise = scrAt
          nfsz
            | isFontPlus = fszAt + 1
            | isFontMinus = fszAt - 1
            | otherwise = fszAt
          nltw
            | isFontPlus = ltwAt + 1
            | isFontMinus = ltwAt - 1
            | otherwise = ltwAt
          nlnw
            | isFontPlus = lnwAt + 1
            | isFontMinus = lnwAt - 1
            | otherwise = lnwAt
          natr
            | isTglDir = if wmdSt==T then atrSt{gps=initYokoPos,scr=V2 0 0} 
                                     else atrSt{gps=initTatePos,scr=V2 0 0} 
            | isTglOsd = if fm==Ost then atrSt{fmd=Got} else atrSt{fmd=Ost}
            | isTglMin = if fm==Min then atrSt{fmd=Got} else atrSt{fmd=Min}
            | otherwise = atrSt{scr=nscr,jmp=(jmp atrSt){sjn=nsjn},fsz=nfsz
                              ,ltw=nltw,lnw=nlnw}
          netx 
            | ised = it 
            | isIns && not ised && it/=T.empty = T.empty
            | otherwise = etxSt
          ncpl
            | isTglColor = if cplSt==length colorPallet - 1 then 0 else cplSt+1 
            | otherwise = cplSt
          ntps
--            | ifmSt = tpsSt
            | isFarForward = tpsFarForward
            | isFarBack = tpsFarBack
            | isToTop = 0
            | isUp = if wmdSt==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
            | isDown = if wmdSt==T 
                        then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
            | isLeft = if wmdSt==Y then if tpsSt==0
                        then 0 else tpsSt-1 else tpsNextLine
            | isRight = if wmdSt==Y 
                        then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
            | isExeDig = tpsSt + T.length (toDig ncom digSt) 
            | isCom = tpsSt + T.length comName 
            | isExeCode = lastTps tpsSt texSt + T.length codeResult
            | isIns && nit/=T.empty && not ised = tpsSt + T.length nit 
            | isBS = if tpsSt>0 then tpsSt-1 else tpsSt
            | isDeleteLine = headTps tpsSt texSt
            | otherwise = tpsSt
          nemd
            | isExeDig = Ins
            | isToIns = Ins 
            | isToNor = Nor 
            | isToDig = Dgr
            | otherwise = emdSt
          nwmd  
            | isTglDir = if wmdSt==T then Y else T
            | otherwise = wmdSt
          ntex
            | ifmSt = texSt
            | isBS && tpsSt>0 = T.take (tpsSt-1) texSt <> T.drop tpsSt texSt
            | isDeleteLine = deleteCurrentLine tpsSt texSt
            | isCom = textIns comName tpsSt texSt
            | isExeCode = textIns codeResult (lastTps tpsSt texSt) texSt 
            | isExeDig = textIns (toDig ncom digSt) tpsSt texSt
            | isIns && not ised = textIns nit tpsSt texSt 
            | otherwise = texSt
          ndts 
            | isDrawClear = []
            | isMousePressed && isc && not (null dtsSt) = 
                 dtsSt ++ addMidDots (last dtsSt) (toDotPos mps scrAt,cplSt)
                       ++ [(toDotPos mps scrAt,cplSt)]
            | isMousePressed = dtsSt++[(toDotPos mps scrAt,cplSt)]  
            | otherwise = dtsSt
          ncod
            | isExeCode && yo==Io = if '\n' `elem` ta then lines ta else [ta] 
            | otherwise = [] 
          ncom
            | isExit || isToTop || isDeleteLine = "" 
            | isDig = if length comSt<2 then comSt ++ T.unpack it else T.unpack it 
            | isNor = case kc of 
                KeycodeD -> comSt ++ "d"
                KeycodeG -> comSt ++ "g"
                _ -> comSt 
            | otherwise = comSt
          nifm
            | isTglFmt = not ifmSt
            | otherwise = ifmSt
          nisk
            | isSkkEdit = True
            | iskSt && it/=T.empty = False 
            | otherwise = iskSt
          ninp
            | isNewFile = NFL
            | isLoadFile = LFL
            | isLoadPrevFile = LPR
            | isLoadRecentFile = LRF
            | isJump = JMP 
            | isJBak = JBK
            | isExeCode && yo==Io = EXE
            | isQuit = QIT
            | isKeyPressed = PKY
            | isMousePressed = PMO
            | otherwise = NON
          nactSt = actSt{tex=ntex,etx=netx,dts=ndts,tps=ntps}
          isTpsUpdate = tpsSt /= ntps
          nst = st{act=nactSt,cdn=cdnSt{cod=ncod},com=ncom,atr=natr
                  ,emd=nemd,wmd=nwmd,cpl=ncpl,ifm=nifm,isk=nisk}
      S.put nst
      when isToIns $ startTextInput (Rect 0 0 50 50)
      when isToNor stopTextInput
      return (1,Just (ninp,isTpsUpdate||ised))

