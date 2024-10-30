module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (Text)
import qualified Data.Text as T
--import qualified SDL.Mixer as M
import Sound.OpenAL.AL.Buffer (Buffer)
import Sound.OpenAL.AL.Source (Source,loopingMode,LoopingMode(..),queueBuffers,play,stop)
--import Data.ObjectName (genObjectName)
import Data.StateVar (($=))
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt)
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture,Surface
                          ,createTextureFromSurface,destroyTexture)
import SDL.Vect (V2(..))
import SDL.Time (delay)
import qualified MyData as MD
import MyAction (getCid)
import General (getLastChar,toList)
import Game.WkDraw (wkDraw)
import Game.WkMap (createMapS)
import Game.WkEvent (exeEvent)
import Game.WkAction (wkInput,makeWkTextData)
import Game.WkData (Waka(..),Input(..),IMode(..),Direction(..),Cha(..),Pos,GMap,MProp(..)
                   ,delayTime,visibleMapSize,plDelay,chaDelay,defaultMapProp)

--wkLoop :: MonadIO m => Renderer -> [Font] -> [[Surface]]
--                                      -> [M.Music] -> S.StateT Waka m () 
wkLoop :: MonadIO m => Renderer -> [Font] -> [[Surface]] -> Texture
                                      -> [Buffer] -> Source -> S.StateT Waka m () 
wkLoop re fonts surfs mapTex muses source = do 
  inp <- wkInput
  wk <- S.get
  let (imsWk,impWk,imuWk) = (ims wk,imp wk,imu wk)
  let isMusicOn = imsWk && not impWk 
  let isMusicOff = imsWk && impWk
  when isMusicOn $ do
    loopingMode source $= Looping
    queueBuffers source [muses!!mfn wk]
    play [source]
     --M.playMusic M.Forever (muses!!(mfn wk))
  when isMusicOff $ stop [source]
  --_ <- if isMusicOff then  M.fadeOutMusic 1000 else return False

  newMapTex <- if imuWk then do
    destroyTexture mapTex
    sf <- createMapS (head surfs) (gmp wk)
    S.put (wk{imu=False})
    createTextureFromSurface re sf
                        else return mapTex

  S.put (wk{ims=False,imp= isMusicOn || (not isMusicOff && impWk)})
  let mdiWk = mdi wk
  case mdiWk of
    TXT -> textMode re fonts surfs newMapTex inp
    PLY -> mapMode re fonts surfs newMapTex inp
    _ -> return ()
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts surfs newMapTex muses source

textMode :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> Texture 
                                            -> Input -> S.StateT Waka m ()
textMode re fonts surfs mapTex inp = do
  wk <- S.get
  let (texWk,stxWk,tpsWk,tmdWk,chsWk) 
          = (tex wk,stx wk,tps wk,tmd wk,chs wk) 
      lch = getLastChar (T.take (tpsWk+1) texWk)
      isStop = lch == '。'
      isEvent = lch == '\\'
      isMap = lch == '~'
      isDialog = tpsWk < T.length texWk
      isShowing = isDialog && not isStop
      eventText = if isEvent then getTargetText getEventLength tpsWk texWk else T.empty
      addTps = if isShowing then calcTps tpsWk texWk else tpsWk
      addText = case lch of
                  '。'  -> T.empty
                  '\\' -> T.empty
                  '~' -> T.empty
                  ';'  -> T.singleton ';'<>getTargetText getComLength tpsWk texWk
                  _    -> T.singleton lch
      nStx = if isShowing then stxWk <> addText else stxWk
      ntps = if isShowing then tpsWk+addTps else tpsWk
      nwk = wk{stx=nStx,tps=ntps}
      textData = makeWkTextData nwk
  wkDraw re fonts surfs mapTex textData nwk
  when isEvent $ liftIO $ print eventText
  let (_,_,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
  let nscr = MD.scr lAtr
  let isStart = isStop && inp==Sp
  let nstx' = if isStart && tmdWk==0 then T.empty else nStx
  let ntps' = if isStart then ntps+1 else ntps 
  let nmsz = if tmdWk==0 then V2 0 0 else visibleMapSize 
  let nchs = zipWith (\cr dl 
        -> cr{cac = let cacCr = cac cr in if cacCr==dl*2 then 0 else cacCr+1})
                                                                    chsWk chaDelay
  let nmdi = if isMap then PLY else TXT 
  let nwk' = nwk{stx=nstx', tps=ntps', scr=nscr, msz=nmsz, mdi=nmdi, chs=nchs}
  S.put nwk'
  when isEvent $ exeEvent eventText

mapMode :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> Texture
                                                 -> Input -> S.StateT Waka m ()
mapMode re fonts surfs mapTex inp = do
  wk <- S.get
  let (chsWk,plnWk,mpsWk,mrpWk,gmpWk,tszWk) 
            = (chs wk,pln wk,mps wk,mrp wk,gmp wk,tsz wk) 
      textData = makeWkTextData wk
      chP = chsWk!!plnWk
      (pdrCh,ppsCh,prpCh,pacCh,pimCh) = (cdr chP, cps chP, crp chP, cac chP, icm chP)
      npdr = case inp of
                Ri -> East
                Up -> North
                Lf -> West
                Dn -> South
                _  -> pdrCh
      npim = inp/=Rl && (inp==Ri || inp==Up || inp==Lf || inp==Dn || pimCh) 
      (nmps,(npps,nprp),iss) =
               charaMove True npim gmpWk mpsWk mrpWk tszWk npdr ppsCh prpCh 
      nchP = chP{cdr=npdr,cps=npps,crp=nprp,icm=npim} 
      chs' = toList chsWk plnWk nchP
      nchs = zipWith (\cr dl 
        -> cr{cac = let cacCr = cac cr in if cacCr==dl*2 then 0 else cacCr+1})
                                                                    chs' chaDelay
      nmrp = if iss then nprp else mrpWk
      nwk = wk{mps=nmps,mrp=nmrp,chs=nchs,isc=iss}
  wkDraw re fonts surfs mapTex textData nwk
  when iss $ liftIO $ putStrLn (show nmps ++ " " ++ show npps ++ " " ++show nprp)
  S.put nwk

type MapPos = Pos
type MapRPos = Pos
type ChaPos = Pos
type ChaRPos = Pos
type TileSize = CInt
type IsPlayer = Bool

charaMove :: IsPlayer -> Bool -> GMap -> MapPos -> MapRPos -> TileSize -> Direction 
                    -> ChaPos -> ChaRPos -> (MapPos,(ChaPos,ChaRPos),Bool) 
charaMove ip im gm mpos@(V2 x y) (V2 rx ry) ts dr cpos@(V2 a b) crps@(V2 p q) =  
  let isFr = isFree gm
      du = div ts 4 
      (V2 msx msy) = V2 (fromIntegral (length (head gm))) (fromIntegral (length gm))
      (V2 vmsx vmsy) = visibleMapSize
      (V2 mpr mpd) = mpos + visibleMapSize - V2 1 1 --map pos right, map pos down
      canMove = im && case dr of
        East -> (p==0 && isFr (V2 (a+1) b) 
                      && (q==0 || (q>0 && isFr (V2 (a+1) (b+1))) 
                               || (q<0 && isFr (V2 (a+1) (b-1))))) || p/=0  
        North -> (q==0 && isFr (V2 a (b-1))
                       && (p==0 || (p>0 && isFr (V2 (a+1) (b-1)))
                                || (p<0 && isFr (V2 (a-1) (b-1))))) || q/=0
        West -> (p==0 && isFr (V2 (a-1) b)
                      && (q==0 || (q>0 && isFr (V2 (a-1) (b+1)))
                               || (q<0 && isFr (V2 (a-1) (b-1))))) || p/=0
        South -> (q==0 && isFr (V2 a (b+1))
                       && (p==0 || (p>0 && isFr (V2 (a+1) (b+1)))
                                || (p<0 && isFr (V2 (a-1) (b+1))))) || q/=0
      (V2 dx dy) = if canMove then case dr of 
              East -> V2 du 0; North -> V2 0 (-du); West -> V2 (-du) 0; South -> V2 0 du 
                              else V2 0 0
      (V2 tp tq) = crps + V2 dx dy
      da = if (mod tp ts == 0) && tp/=0 then div tp ts else 0
      db = if (mod tq ts == 0) && tq/=0 then div tq ts else 0
      (V2 ta tb) = cpos + V2 da db
      ntp = if abs tp==ts then 0 else tp
      ntq = if abs tq==ts then 0 else tq
      isInRect = case dr of 
                  East -> ta < mpr || (ta==mpr && ntp==0) 
                  North -> tb > y || (tb==y && ntq==0)
                  West -> ta > x || (ta==x && ntp==0)
                  South -> tb < mpd || (tb==mpd && ntq==0)
      isScroll = ip && case dr of
                  East -> (x+vmsx < msx) && ((ta==x+vmsx-2 && ntp>0) || ta==x+vmsx-1)
                  North -> (y > 0) && ((tb==y+1 && ntq<0) || tb==y)
                  West -> (x > 0) && ((ta==x+1 && ntp<0) || ta==x)
                  South -> (y+vmsy < msy) && ((tb==y+vmsy-2 && ntq>0) || tb==y+vmsy-1)
      ncpos = if not ip || isInRect then V2 ta tb else cpos              
      ncrps = if not ip || isInRect then V2 ntp ntq else crps
      nmpos = if isInRect && isScroll then mpos + V2 da db else mpos
   in (nmpos,(ncpos,ncrps),isScroll) 

isFree :: GMap -> ChaPos -> Bool
isFree gm (V2 a b) = (a>=0 && b>=0) &&
    (let mProp = defaultMapProp!!read [(gm!!fromIntegral b)!!fromIntegral a]
      in mProp/=Bl)

calcTps :: Int -> Text -> Int 
calcTps tpsWk texWk =
  let beforeTpsText = T.take (tpsWk+1) texWk
      afterTpsText = T.drop (tpsWk+1) texWk
      ch = getLastChar beforeTpsText 
   in case ch of
        ';' -> 1 + getComLength afterTpsText 
        '\\' -> 1 + getEventLength afterTpsText 
        _   -> 1 

getTargetText :: (Text -> Int) -> Int -> Text -> Text
getTargetText getLengthF tpsWk texWk =
  let afterTpsText = T.drop (tpsWk+1) texWk
      textLength = getLengthF afterTpsText
   in T.take textLength afterTpsText 

getEventLength :: Text -> Int
getEventLength tx =
  let txs = T.lines tx
      num = if null txs then 0 else T.length (head txs)
   in if length txs < 2 then num else num+1

getComLength :: Text -> Int
getComLength tx =
  let (cidWk,_) = getCid tx 
   in spaceNum cidWk tx

spaceNum :: Int -> Text -> Int
spaceNum (-1) _ = -1
spaceNum i tx =
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx)
      ni = if ch==' ' then i-1 else i
   in if tx==T.empty then 0 else 1 + spaceNum ni txs
