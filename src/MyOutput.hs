{-# LANGUAGE OverloadedStrings #-}
module MyOutput (myOut) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Linear.V2 (V2(..))
import MyData (State(..),Active(..),Attr(..),Coding(..),Jumping(..),Dot,Input(..)
              ,JBak,FrJp,TextData(..)
              ,delayTime,textFileName,textPosFile,dotFileName,jumpNameFile)
import MyAction (beforeDraw,afterDraw,makeTextData)
import MyLib (textToDots,dotsToText,jumpsToText)
import MyFile (fileRead,fileWrite)
import MyCode (exeCode)
import Connector(Font,Renderer,Texture,delay,myDraw)
import General (isLastElem)
import Game.WkMain (runWaka)

data FC = NewFile | LoadNextFile | LoadPrevFile | LoadRecentFile | LoadFile 
        | JumpFile | JumpBackFile | RunWaka | DoNothing deriving Eq

type HaText = T.Text
type FileNum = Int
type TextPos = Int
type Dots = [Dot]
type JBacks = [JBak]
data CFile = CFile !HaText !FileNum !TextPos !Dots !Bool !JBacks

myOut :: Renderer -> [Font] -> [Texture] -> Bool -> (Input,Bool) 
                                                -> S.StateT State IO Bool 
myOut re fonts itexs _ (inp,isUpdateTps) = do
  st <- S.get
  let actSt = act st
      nicr = isUpdateTps || icr actSt
      ncrc = if isUpdateTps then 0 else crc actSt
      bst = beforeDraw st{act=actSt{crc=ncrc,icr=nicr},cdn=(cdn st){ipr=True}}
      bcdnSt = cdn bst
  S.put bst

  when (inp==EXE) $ mapM_ exeCode (cod bcdnSt) 

  cst <- S.get
  let ccdnSt = cdn cst
      msgSt = msg ccdnSt
  when (isLastElem msgSt "codeExe") $ mapM_ exeCode (cod ccdnSt) 
  
  cst' <- S.get
  let cactSt' = act cst'
      isUpdateText = tex actSt /= tex cactSt' || icr actSt /= icr cactSt' 
          || isUpdateTps || inp==PKY || iup cst' || etx actSt /= etx cactSt' 
      isUpdateDraw = inp==PMO || inp==EXE || isUpdateText
      isOnlyMouse = inp==PMO && not isUpdateText
      textDatas = if isUpdateDraw then makeTextData cst' else []
      getAtr d = let (TD _ _ gatr _) = last d in gatr
      natr = if null textDatas then atr cst' else getAtr textDatas
      nscr = if inp==NFL || inp==LFL || inp==JMP then V2 0 0 else scr natr
      jmpAt = jmp natr
      (njps,nfjp,jbkAt,nsjn,fpsSt) = 
          (jps jmpAt, fjp jmpAt, jbk jmpAt, sjn jmpAt, fps cactSt')

      isLoadTgt = isLastElem msgSt "loadFile"
      tFjp = if isLoadTgt then read (last (init msgSt)) else 0
      isRunWaka = isLastElem msgSt "runWaka"
      wkInitFile = if isRunWaka then read (last (init msgSt)) else 0

  when isUpdateDraw $ myDraw re fonts itexs textDatas isOnlyMouse (beforeDraw cst')

  let fc
       | inp==NFL = NewFile
       | inp==LFL = LoadNextFile
       | inp==LPR = LoadPrevFile
       | inp==LRF = LoadRecentFile
       | isLoadTgt = LoadFile
       | inp==JMP = JumpFile
       | inp==JBK = JumpBackFile
       | isRunWaka = RunWaka
       | otherwise = DoNothing
  CFile ntex nfps ntps ndts niup njbk <- case fc of
        NewFile -> newFile fpsSt jbkAt cst' 
        LoadNextFile -> loadNextFile fpsSt jbkAt cst' 
        LoadPrevFile -> lastFileNum >>= \lfn -> do 
            let prevFileNum = if fpsSt==0 then lfn else fpsSt-1
            loadFile fpsSt prevFileNum jbkAt cst'
        LoadRecentFile -> lastFileNum >>= \lfn -> loadFile fpsSt lfn jbkAt cst' 
        LoadFile -> loadFile fpsSt tFjp jbkAt cst'
        JumpFile -> jumpFile nfjp jbkAt nsjn fpsSt cst' 
        JumpBackFile -> jumpBackFile jbkAt fpsSt cst'
        _other -> return (CFile ((tex.act) cst') fpsSt ((tps.act) cst') ((dts.act) cst') False jbkAt)

  when (fc==RunWaka) $ runWaka wkInitFile "initWaka"

  let nactSt = cactSt'{tex=ntex,dts=ndts,fps=nfps,tps=ntps}
  let catrSt' = atr cst'
  let nst = afterDraw cst'{act=nactSt,cdn=(cdn cst'){msg=[]},atr=catrSt'{scr=nscr,jmp=(jmp catrSt'){jps=njps,fjp=nfjp,jbk=njbk,sjn=nsjn}},iup=niup}

  delay delayTime

  S.put nst 
  if inp==QIT then do 
    fileWriteR fpsSt nst
    fileWrite textPosFile (T.pack$unwords [show ((fps.act) nst),show ((tps.act) nst)])
    fileWrite jumpNameFile (jumpsToText njps)
    return True 
              else return False

newFile :: (MonadIO m) => Int -> [JBak] -> State -> m CFile 
newFile fpsSt jbkAt st = do
    fileWriteR fpsSt st
    nextFileNum <- nextNewFileNum (fpsSt + 1)
    return (CFile T.empty nextFileNum 0 [] True jbkAt)

loadNextFile :: (MonadIO m) => Int -> [JBak] -> State -> m CFile
loadNextFile fpsSt jbkAt st = do
    fileWriteR fpsSt st
    loadFileNum <- loadExistFileNum (fpsSt + 1)
    (loadText, dots) <- fileReadR loadFileNum
    return (CFile loadText loadFileNum 0 dots True jbkAt)

loadFile :: (MonadIO m) => Int -> Int -> [JBak] -> State -> m CFile
loadFile fpsSt tFjp jbkAt st = do
    fileWriteR fpsSt st
    loadFileNum <- loadExistFileNum tFjp 
    (loadText, dots) <- fileReadR loadFileNum
    return (CFile loadText loadFileNum 0 dots True jbkAt)

jumpFile :: (MonadIO m) => [FrJp] -> [JBak] -> Int -> Int -> State -> m CFile
jumpFile fjpAt jbkAt sjnAt fpsSt st = do
      let (loadFileNum,textPos) = snd$fjpAt!!sjnAt
      fileWriteR fpsSt st
      (loadText, dots) <- fileReadR loadFileNum
      let jb = jbkAt ++ [(fpsSt,(tps.act) st)]
      return (CFile loadText loadFileNum textPos dots True jb)

jumpBackFile :: (MonadIO m) => [JBak] -> Int -> State -> m CFile
jumpBackFile jbkAt fpsSt st = do
      let canJBack = not (null jbkAt)
          (loadFileNum,textPos) = if canJBack then last jbkAt else (fpsSt,(tps.act) st)
          nextFileName = textFileName++show loadFileNum++".txt"
          nextDotFile = dotFileName++show loadFileNum++".txt"
      loadText <- if canJBack then fileRead nextFileName else return ((tex.act) st)
      loadDotText <- if canJBack then fileRead nextDotFile else return T.empty 
      let dots = if canJBack then textToDots (T.words loadDotText) else (dts.act) st
      return (CFile loadText loadFileNum textPos dots True (if canJBack then init jbkAt else jbkAt))

nextNewFileNum :: (MonadIO m) => Int -> m Int
nextNewFileNum i = do 
  let fileName = textFileName++show i++".txt" 
  fileExist <- liftIO $ doesFileExist fileName 
  if fileExist then nextNewFileNum (i+1) else return i

loadExistFileNum :: (MonadIO m) => Int -> m Int
loadExistFileNum i = do
  let fileName = textFileName++show i++".txt"
  fileExist <- liftIO $ doesFileExist fileName
  if fileExist then return i else loadExistFileNum 0

lastFileNum :: (MonadIO m) => m Int
lastFileNum = lastFileNum' 0

lastFileNum' :: (MonadIO m) => Int -> m Int
lastFileNum' i = do
  let fileName = textFileName++show i++".txt"
  fileExist <- liftIO $ doesFileExist fileName
  if fileExist then lastFileNum' (i+1) else return (i-1)

fileWriteR :: (MonadIO m) => Int -> State -> m ()
fileWriteR fp st = do
  fileWrite (textFileName++show fp++".txt") ((tex.act) st)
  fileWrite (dotFileName++show fp++".txt") ((dotsToText.dts.act) st)

fileReadR :: (MonadIO m) => Int -> m (T.Text, [Dot])
fileReadR lfn = do
  let nextFileName = textFileName++show lfn++".txt"
      nextDotFile = dotFileName++show lfn++".txt"
  loadText <- fileRead nextFileName  
  loadDotText <- fileRead nextDotFile
  let dots = textToDots (T.words loadDotText)
  return (loadText, dots)
