{-# Language OverloadedStrings #-}
module MyCode(exeCode) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (when)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Linear.V2 (V2(..))
import MyData (State(..),Active(..),Coding(..),Code
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..))
import Mana.Mana (evalCode,taiyouMn,Yo(..),Dtype(..),preDef,userDef)
import MyLib (textIns,lastTps,takeCodes)
import General (getIndex,delIndex)

type StateIO = S.StateT State IO ()
type Func = [String] -> StateIO

exeCode :: Code -> StateIO 
exeCode code = do
  let cds = words code 
      (arg,funcName) = (init cds, last cds)
  fromMaybe idf (lookup funcName funcs) arg -- idf is a function to do nothing 
  iprSt <- S.get >>= return.ipr.cdn
  when iprSt $ addTex "OK." 

addTex :: T.Text -> StateIO
addTex tx = do 
  st <- S.get
  let actSt = act st 
      (texSt,tpsSt) = (tex actSt,tps actSt)
      insTx = "\n"<>tx                      -- insert text
      lTps = lastTps tpsSt texSt            -- lastTps <-- MyLib.hs
      ntex = textIns insTx lTps texSt       -- textIns <-- MyLib.hs
      ntps = lTps + T.length insTx 
      nact = actSt{tex=ntex,tps=ntps}
      nst = st{act=nact, cdn=(cdn st){ipr = False}}
  S.put nst

getMoz :: String -> String
getMoz mz = if length mz > 2 then tail$init mz else mz

funcs :: [(String,Func)]
funcs = [("cls",cls),("clear",clear)
        ,("color",color),("lineSize",lineSize)
        ,("drawRect",drawRect),("drawLine",drawLine)
        ,("drawCircle",drawCircle),("drawDot",drawDot),("drawGrid",drawGrid)
        ,("drawImage",drawImage),("load",load),("waka",waka),("run",run),("ha",ha)]

idf :: [String] -> StateIO
idf _  = return () 

cls :: [String] -> StateIO
cls _  = S.get >>= 
    (\st -> return st{act=(act st){tex=T.empty,tps=0},cdn=(cdn st){ipr=False}}) >>= S.put

clear :: [String] -> StateIO
clear _  = S.get >>= (\st -> return st{drw=[],img=[]}) >>= S.put

color :: [String] -> StateIO
color [x] = S.get >>= (\st -> return st{cpl=read x}) >>= S.put
color _ = return () 

lineSize :: [String] -> StateIO
lineSize [x] = S.get >>= (\st -> return st{lsz=read x}) >>= S.put
lineSize _ = return () 

putDraw :: Shp -> StateIO
putDraw shp = do
  st <- S.get
  let (cn,sz,drwSt) = (cpl st, lsz st, drw st)
      ndrw = Drw cn sz shp
  S.put st{drw=drwSt++[ndrw]}

drawRect :: [String] -> StateIO
drawRect [a,b,c,d,e]  = let isFill = getMoz a=="f"
                         in putDraw (R (Rc isFill (V2 (read b) (read c)) (V2 (read d) (read e))))
drawRect _ = return () 

drawLine :: [String] -> StateIO
drawLine [a,b,c,d]  = putDraw (L (Li (V2 (read a) (read b)) (V2 (read c) (read d))))
drawLine _ = return () 

drawCircle :: [String] -> StateIO
drawCircle [a,b,c,d]  = putDraw (C (Cr (getMoz a=="f") (V2 (read b) (read c)) (read d)))
drawCircle _ = return ()  

drawDot :: [String] -> StateIO
drawDot [a,b] = putDraw (D (Dt (V2 (read a) (read b))))
drawDot _  = return () 

drawGrid :: [String] -> StateIO
drawGrid args = when (length args == 6) $ do 
    let [a,b,c,d,e,f] = map read args 
        dw = e `div` a
        dh = f `div` b
    mapM_ ((\x -> putDraw (L (Li (V2 (c+x) d) (V2 (c+x) (d+f))))) . (dw *)) [0..a]
    mapM_ ((\y -> putDraw (L (Li (V2 c (d+y)) (V2 (c+e) (d+y))))) . (dh *)) [0..b]

drawImage :: [String] -> StateIO
drawImage [a,b,c,d,e,f] = do 
  st <- S.get
  let imgSt = img st
      nimg = Img (V2 (read a) (read b)) (V2 (read c) (read d)) (read e) (getMoz f) 
  S.put st{img=imgSt++[nimg]}
drawImage _  = return () 

load :: [String] -> StateIO
load [a] = S.get >>= 
    (\st -> return st{cdn=(cdn st){msg=[a,"loadFile"],ipr=False}}) >>= S.put  
load _ = return ()

waka :: [String] -> StateIO
waka [a] = S.get >>=
    (\st -> return st{cdn=(cdn st){msg=[a,"runWaka"],ipr=False}}) >>= S.put

run :: [String] -> StateIO
run _ = do
  st <- S.get
  let codes = takeCodes ((tex.act) st)
      dfnSt = (dfn.cdn) st
      manas = map (taiyouMn.evalCode (preDef++[(User,userDef++dfnSt)])) codes
      ioCodes = map fst $ filter (\(_,y) -> y==Io) manas
      --results = map fst $ filter (\(_,y) -> y/=Io) manas
  S.put st{cdn=(cdn st){cod=ioCodes, msg=["codeExe"]}}

strYo :: [(String,Yo)]
strYo = [("k",Kaz),("m",Moz),("i",Io)]

readYo :: String -> Yo
readYo str = fromMaybe (read str) (lookup str strYo)

ha :: [String] -> StateIO
ha [a,b] = do
  st <- S.get
  let (lfts,rits) = (splitOn "," a, splitOn "," b)
      (tgts,pyos) = break (=="::") lfts
      yos = if null pyos then gessYos tgts (T.pack (unwords rits)) else map readYo (tail pyos)
      tgtStr = unwords tgts
      cdnSt = cdn st
      dfnSt = dfn cdnSt 
      tgtList = if null dfnSt then [] else map (fst.fst) dfnSt
      dfnSt' = if not (null dfnSt) && tgtStr `elem` tgtList 
                  then delIndex (getIndex tgtStr tgtList) dfnSt else dfnSt
      ndfn = ((tgtStr,yos),unwords rits)
  S.put st{cdn=cdnSt{dfn=dfnSt'++[ndfn]}} 
ha _ = return () 

gessYos :: [String] -> T.Text -> [Yo]
gessYos lfs rt = [] 

