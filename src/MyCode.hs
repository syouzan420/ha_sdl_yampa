{-# Language OverloadedStrings #-}
module MyCode(exeCode) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Linear.V2 (V2(..))
import MyData (State(..),Active(..),Coding(..),Code
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..))
import Mana.Mana (evalCode,taiyouMn,Yo(..),Dtype(..),preDef,userDef)
import MyLib (textIns,lastTps,takeCodes)
import General (getIndex,delIndex)

type Func = [String] -> State -> State

exeCode :: Code -> State -> State 
exeCode code st = do
  let cds = words code 
      (arg,funcName) = (init cds, last cds)
      nst = fromMaybe idf (lookup funcName funcs) arg st 
      iprSt = (ipr.cdn) nst
   in if iprSt then addTex "OK." nst else nst

addTex :: T.Text -> State -> State
addTex tx st = do 
  let actSt = act st 
      (texSt,tpsSt) = (tex actSt,tps actSt)
      insTx = "\n"<>tx                      -- insert text
      lTps = lastTps tpsSt texSt            -- lastTps <-- MyLib.hs
      ntex = textIns insTx lTps texSt       -- textIns <-- MyLib.hs
      ntps = lTps + T.length insTx 
      nact = actSt{tex=ntex,tps=ntps}
   in st{act=nact, cdn=(cdn st){ipr = False}}

getMoz :: String -> String
getMoz mz = if length mz > 2 then tail$init mz else mz

funcs :: [(String,Func)]
funcs = [("cls",cls),("clear",clear)
        ,("color",color),("lineSize",lineSize)
        ,("drawRect",drawRect),("drawLine",drawLine)
        ,("drawCircle",drawCircle),("drawDot",drawDot),("drawGrid",drawGrid)
        ,("drawImage",drawImage),("load",load),("waka",waka),("run",run),("ha",ha)]

idf :: [String] -> State -> State
idf _ st = st 

cls :: [String] -> State -> State
cls _ st = st{act=(act st){tex=T.empty,tps=0},cdn=(cdn st){ipr=False}} 

clear :: [String] -> State -> State
clear _ st = st{drw=[],img=[]}

color :: [String] -> State -> State
color [x] st = st{cpl=read x}
color _ st   = st 

lineSize :: [String] -> State -> State
lineSize [x] st = st{lsz=read x}
lineSize _ st   = st 

putDraw :: Shp -> State -> State
putDraw shp st = 
  let (cn,sz,drwSt) = (cpl st, lsz st, drw st)
      ndrw = Drw cn sz shp
   in st{drw=drwSt++[ndrw]}

drawRect :: [String] -> State -> State
drawRect [a,b,c,d,e] st = 
  let isFill = getMoz a=="f"
   in putDraw (R (Rc isFill (V2 (read b) (read c)) (V2 (read d) (read e)))) st
drawRect _ st = st 

drawLine :: [String] -> State -> State
drawLine [a,b,c,d] st = 
  putDraw (L (Li (V2 (read a) (read b)) (V2 (read c) (read d)))) st
drawLine _ st = st 

drawCircle :: [String] -> State -> State
drawCircle [a,b,c,d] st = 
  putDraw (C (Cr (getMoz a=="f") (V2 (read b) (read c)) (read d))) st
drawCircle _ st = st  

drawDot :: [String] -> State -> State
drawDot [a,b] st = putDraw (D (Dt (V2 (read a) (read b)))) st
drawDot _ st = st 

drawGrid :: [String] -> State -> State
drawGrid args st = case map read args of 
  [a,b,c,d,e,f] ->  
    let dw = e `div` a
        dh = f `div` b
        nst = foldr ((\x s -> 
          putDraw (L (Li (V2 (c+x) d) (V2 (c+x) (d+f)))) s) . (dw *)) st [0..a]
     in foldr ((\y s -> 
          putDraw (L (Li (V2 c (d+y)) (V2 (c+e) (d+y)))) s) . (dh *)) nst [0..b]
  _else -> st 

drawImage :: [String] -> State -> State
drawImage [a,b,c,d,e,f] st =  
  let imgSt = img st
      nimg = Img (V2 (read a) (read b)) (V2 (read c) (read d)) (read e) (getMoz f) 
   in st{img=imgSt++[nimg]}
drawImage _  st = st 

load :: [String] -> State -> State
load [a] st = st{cdn=(cdn st){msg=[a,"loadFile"],ipr=False}}  
load _ st = st 

waka :: [String] -> State -> State
waka [a] st = st{cdn=(cdn st){msg=[a,"runWaka"],ipr=False}}
waka _ st = st 

run :: [String] -> State -> State
run _ st = 
  let codes = takeCodes ((tex.act) st)
      dfnSt = (dfn.cdn) st
      manas = map (taiyouMn.evalCode (preDef++[(User,userDef++dfnSt)])) codes
      ioCodes = map fst $ filter (\(_,y) -> y==Io) manas
   in st{cdn=(cdn st){cod=ioCodes, msg=["codeExe"]}}

strYo :: [(String,Yo)]
strYo = [("k",Kaz),("m",Moz),("i",Io)]

readYo :: String -> Yo
readYo str = fromMaybe (read str) (lookup str strYo)

ha :: [String] -> State -> State
ha [a,b] st = 
  let (lfts,rits) = (splitOn "," a, splitOn "," b)
      (tgts,pyos) = break (=="::") lfts
      yos = if null pyos then gessYos tgts (T.pack (unwords rits)) 
                         else map readYo (tail pyos)
      tgtStr = unwords tgts
      cdnSt = cdn st
      dfnSt = dfn cdnSt 
      tgtList = if null dfnSt then [] else map (fst.fst) dfnSt
      dfnSt' = if not (null dfnSt) && tgtStr `elem` tgtList 
                  then delIndex (getIndex tgtStr tgtList) dfnSt else dfnSt
      ndfn = ((tgtStr,yos),unwords rits)
   in st{cdn=cdnSt{dfn=dfnSt'++[ndfn]}} 
ha _ st = st 

gessYos :: [String] -> T.Text -> [Yo]
gessYos _lfs _rt = [] 

