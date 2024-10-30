{-# LANGUAGE OverloadedStrings #-}
module Mana.Mana (evalCode,makeManas,addSpaces,taiyouMn,makeStrings, Mn(..), Ta, Yo(..),Definition,Df(..),Dtype(..),DefList,setDf,preDef,userDef)  where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Tree (Tree(..), Forest)
import Data.Maybe (fromMaybe, isJust)
import Mana.MyTree (Elm(..),L,R(..),numR,mtR,ltR,addElem,showF)
import General (getIndex)
  
type Ta = String 
data Yo = Kaz | Moz | Io | Def | Spe | Var deriving (Eq, Show, Read) 
data Mn = Mn Ta Yo deriving Eq
type LR = ([L],[R])
type Definition = ((String,[Yo]),String)
data Dtype = Prim | PrIo | User | UsIo | Non deriving (Eq, Show)
type DefList = [(Dtype,[Definition])]
data Df = Df Dtype String [Yo] String deriving (Eq, Show)

instance Show Mn where
  show (Mn t y) = t 
--     ++ "-" ++
--     case y of Kaz -> "K"; Moz -> "M"; Io -> "I"; Def -> "D"; Spe -> "S"; Var -> "V"

evalCode :: DefList -> T.Text -> Mn
evalCode dfl = makeMana dfl . fst . makeManas (concatMap snd dfl)

showCode :: T.Text -> IO ()
showCode = showFLR . makeManas (concatMap snd preDef)

codeToString :: T.Text -> String
codeToString = manasToString . fst . makeManas (concatMap snd preDef)
--codeToString = manasToString . (: []) . last . fst . makeManas (concatMap snd preDef)
--codeToString = manasToString . init . fst . makeManas (concatMap snd preDef)

codeToYos :: T.Text -> [Yo]
codeToYos = manasToYos . fst .makeManas (concatMap snd preDef)

manasToString :: Forest Mn -> String
manasToString [] = []
manasToString (Node (Mn t _) sf:xs) = t ++","++ manasToString sf ++ manasToString xs 

manasToYos :: Forest Mn -> [Yo]
manasToYos [] = []
manasToYos (Node (Mn _ y) sf:xs) = y : manasToYos sf ++ manasToYos xs

setDf :: DefList -> String -> Df
setDf dfl name = fromMaybe (Df Non "" [] "") (searchFromDef name dfl)

howManyElem :: Eq a => a -> [a] -> Int
howManyElem e = foldl (\acc x -> if x==e then acc+1 else acc) 0 

getYo :: [Definition] -> String -> Yo
getYo def x | isDef def x = Def | isMoz x = Moz | isKaz x = Kaz | isSpe x = Spe | otherwise = Var

showFLR :: (Forest Mn,LR) -> IO () 
showFLR (fr,lr) = putStrLn (showF fr ++ "\n" ++ show lr)

getTa :: Tree Mn -> Ta
getTa (Node (Mn t _) _) = t

taiyouMn :: Mn -> (Ta,Yo)
taiyouMn (Mn t y) = (t,y) 

getManaFromTree :: Tree Mn -> Mn
getManaFromTree (Node m _) = m

getManaFromTree' :: DefList -> Tree Mn -> Mn
getManaFromTree' _ (Node m []) = m
getManaFromTree' dfl (Node m fm) = makeMana dfl [Node m fm]

searchFromDef :: String -> DefList -> Maybe Df 
searchFromDef _ [] = Nothing 
searchFromDef nm ((x,y):xs) = let dt = searchFromDef' nm y
                                  ((dp,dy),dc) = fromMaybe (("",[]),"") dt
                               in if dp=="" then searchFromDef nm xs else Just (Df x dp dy dc)

searchFromDef' :: String -> [Definition] -> Maybe Definition
searchFromDef' _ [] = Nothing 
searchFromDef' nm (df:xs) =
  if name==nm then Just df else searchFromDef' nm xs
    where name = getName ((fst.fst) df)

defForest :: DefList -> Forest Mn -> Maybe Df 
defForest dfl fm = let mnList = map getManaFromTree fm
                       (taList,yoList) = unzip$map taiyouMn mnList
                       isdef = Def `elem` yoList
                       ind = if isdef then getIndex Def yoList else (-1)
                       name = if isdef then taList!!ind else ""
                    in if isdef then searchFromDef name dfl 
                            else Nothing 

evalDef :: DefList -> Forest Mn -> Mn 
evalDef dfl fm = 
  let Df dt dp dy dc = fromMaybe (Df Non "" [] "") (defForest dfl fm)
      dfs = concatMap snd dfl
      dpList = words dp
      dcList = if dt==Prim || dt==PrIo then words dc else (makeStrings.T.pack) dc
      mnList = map (getManaFromTree' dfl) fm
      (taList,yoList) = unzip$filter (\(t,_) -> t /= ")")$map taiyouMn mnList
      isNumMatch = length yoList == length dy
      yos = zip yoList dy
      isYoMatch = isNumMatch && foldl (\acc (y,yc) -> acc && (y==Def || y==yc)) True yos
      knv = zip dpList taList
      evs
        | isYoMatch = map (\x -> fromMaybe x (lookup x knv)) dcList
        | isNumMatch = ["yos don't match"]
        | otherwise = ["need more arguments"]
      yo = if isNumMatch then fromMaybe Moz (lookup Def yos) else Spe
      rsl 
        | dt==Prim = Mn (preFunc evs) yo 
        | dt==PrIo = Mn (unwords evs) yo
        | otherwise = makeMana dfl $ fst $ makeManas dfs (T.pack$unwords evs)
   in rsl
                
makeMana :: DefList -> Forest Mn -> Mn
makeMana _ [] = Mn "" Moz
makeMana dfl [Node x []] 
  | isJust (defForest dfl [Node x []]) = evalDef dfl [Node x []]
  | otherwise = x 
makeMana dfl (Node (Mn "(" _) y0 : Node (Mn ")" _) y1 : xs)
                            = makeMana dfl (Node (makeMana dfl y0) y1 : xs)
makeMana dfl (Node mn0@(Mn t0 y0) [] : Node mn1@(Mn t1 y1) [] : xs)
  | isJust (defForest dfl [Node mn0 []]) =
    makeMana dfl (Node (evalDef dfl [Node mn0 []]) [] : Node mn1 [] : xs)
  | isJust (defForest dfl [Node mn1 []]) =
    makeMana dfl (Node mn0 [] : Node (evalDef dfl [Node mn1 []]) [] : xs)
  | y0==y1 = case y0 of
      Kaz -> makeMana dfl $ Node (Mn (show (read t0 + read t1)) Kaz) []:xs
      Moz -> makeMana dfl $ Node (Mn (init t0 ++ tail t1) Moz) [] : xs
      Io  -> makeMana dfl $ Node (Mn (t0 ++"\n"++ t1) Io) [] : xs
      _ -> makeMana dfl xs 
  | t0 == ")" = makeMana dfl $ Node (Mn t1 y1) [] : xs
  | t1 == ")" = makeMana dfl $ Node (Mn t0 y0) [] : xs
  | t0 == "(" = makeMana dfl $ Node (Mn t1 y1) [] : xs
  | t1 == "(" = makeMana dfl $ Node (Mn t0 y0) [] : xs
  | otherwise = Mn "Error" Spe 
makeMana dfl (Node mn [] : xs) = makeMana dfl [Node mn [], Node (makeMana dfl xs) []]
makeMana dfl (Node x y : xs) 
  | null xs && getTa (last y) == "=" = Mn (makeHaString (Node x y)) Io 
  | isJust (defForest dfl nfm) = makeMana dfl (Node (evalDef dfl nfm) [] : xs)
  | otherwise = makeMana dfl (Node (makeMana dfl nfm) [] : xs)
   where nfm = let Node x' y' = head y
                in if null y' || isJust (defForest dfl y') || fst (taiyouMn x')=="("
                      then Node x [] : y else Node x (Node x' []:y') : tail y 
 --  where nfm = if fst (taiyouMn x)=="(" then y else Node x [] : y
makeHaString :: Tree Mn -> String
makeHaString (Node x y) = init (manasToString [Node x (init y)]) ++ " " 
                        ++ init (drop 2 (manasToString [last y])) ++ " ha"

makeManas :: [Definition] -> T.Text -> (Forest Mn,LR)
makeManas defs = makeManas' defs ([],[]) [] . makeStrings 

calcL :: String -> [String] -> Int -> Int -> Int
calcL _ [] _ acc = acc  
calcL _ _ 0 acc = acc
calcL s (x:xs) i acc = if s==x then calcL s xs i (acc+1) else calcL s xs (i-1) (acc+1)

makeManas' :: [Definition] -> LR -> Forest Mn -> [String] -> (Forest Mn,LR)
makeManas' _ lr mns [] = (mns,lr)
makeManas' defs (pl,pr) mns (x:xs) = 
  let you = getYo defs x 
      mnl = length mns
      (ls,rs) = if you == Def then getLR defs x (pl,pr) else (pl,pr)
      (l,ls')
        | x == "=" = (mnl,[])
        | null ls = (0,[])
        | head ls < 2 = (head ls,tail ls)
        | otherwise = let (l',tls) = (head ls,tail ls)
                          revTas = map getTa (reverse mns) 
                       in (calcL ")" revTas l' 0,tls)
      (r,rs') 
        | x == "=" = (Rc,[])
        | null rs = (Ri 0,[])
        | x == ")" = let hr = head rs; ir = numR hr
                      in if ltR hr 1 then 
                           if head (tail rs) == Rc then (Ri (ir-1),tail$tail rs) 
                                                   else (Ri (ir-1),tail rs) 
                                     else 
                           if not (null (tail rs)) && hr == Rc
                                          then let hr' = head (tail rs) 
                                                   ir' = numR hr'
                                                in if hr'==Rc then (Ri 0, tail rs)
                                                              else (Ri 0,Ri (ir'-1):tail (tail rs)) 
                                          else (Ri 0, tail rs)
        | otherwise = (head rs,tail rs)
      nl 
        | mnl < l = (l - mnl):ls'
        | x == "(" || x == "=" = (-1):ls'
        | otherwise = if null ls' then ls' else if head ls'==(-1) then 0:ls' else ls'
      nr 
        | you /= Def && you /= Spe && mtR r 0 =
            let ri = numR r - 1 
             in if ri /= 0 then Ri ri:rs'
                           else 
                  if null rs' then Ri 0:rs' 
                              else let hr' = head rs' ; ri' = numR hr'
                                    in if ltR hr' 1 then Ri (ri'-1):tail rs' 
                                                    else Ri ri:rs'
        | x == "(" = if null rs then Rc:rs else if numR r==0 then Rc:rs' else Rc:rs
        | ltR r 1 = rs'
        | r == Rc = r:rs'
        | otherwise = rs
      nmns = addElem (El (Mn x you) l r) mns 
  in makeManas' defs (nl,nr) nmns xs  

getLR :: [Definition] -> String -> LR -> LR
getLR defs df (pl,pr) = let names = map (getName . fst . fst) defs 
                            ind = getIndex df names
                            defws = words$fst$fst$defs!!ind
                            wsLng = length defws
                            nmInd = getIndex df defws
                            npl = if nmInd==0 then pl else nmInd:pl
                            nhpr = wsLng - nmInd - 1
                            npr = if nhpr==0 then pr else Ri nhpr:pr
                         in (npl, npr) 

isMoz :: String -> Bool
isMoz [] = False
isMoz [_] = False 
isMoz (h:tl) = h=='\"' && last tl=='\"'

isKaz :: String -> Bool
isKaz [] = False
isKaz [x] = isDigit x
isKaz (h:tl) = (h=='+' || h=='-' || isDigit h) && all isDigit tl

isDef :: [Definition] -> String -> Bool
isDef _ [] = False
isDef defs str = str `elem` map (getName . fst . fst) defs 

isSpe :: String -> Bool
isSpe [] = False
isSpe str = str `elem` speDef

makeStrings :: T.Text -> [String]
makeStrings = words . T.unpack . addSpaces . forMath
--makeStrings  =  concatMap (\wd -> if isMoz wd then [wd] else (words . T.unpack . addSpaces . T.pack) wd) .  words . T.unpack . forMath 

addSpaces :: T.Text -> T.Text
addSpaces txt =
  foldl (\acc nm -> T.replace nm (" "<>nm<>" ") acc) txt (map T.pack needSpace)

forMath :: T.Text -> T.Text
forMath = T.replace "+" " " . T.replace "-" " -"

getName :: String -> String
getName def = let ws = words def
                  searchNameList = filter (`notElem` usedForArgs) ws
               in if null searchNameList then if null ws then "" else head ws 
                                         else head searchNameList

usedForArgs :: [String]
usedForArgs = ["a","b","c","d","e","f","g","h"]

preDef :: DefList 
preDef = [(Prim,primDef),(PrIo,prioDef)]

--nameDef :: [Definition]
--nameDef = primDef ++ map (\x -> ((x,[Spe]),"")) speDef ++ prioDef ++ userDef

primDef :: [Definition]
primDef = [(("a x b",[Kaz, Kaz, Kaz]),"a b pro"),(("a * b",[Kaz, Kaz, Kaz]),"a b pro")]

userDef :: [Definition]
userDef = [(("a bon b",[Kaz, Kaz, Kaz]),"a bxa")
          ,(("a grid",[Kaz, Io]),"a a 100 100 64xa 64xa drawGrid")
          ,(("a b c d block",[Kaz,Kaz,Kaz,Moz,Io]),"(100 ax64) (100 bx64) 64 64 cx90 (\"block_\" d) drawImage")
          ,(("a b c nanika",[Kaz,Kaz,Kaz,Io]),"a color b lineSize 100 100 (100 c) (100 c) drawLine")]


--color a: color number
--lineSize a: line size (thickness) (CInt)
--drawRect a: fill("f") or not, (b,c): startPosition(upper left), (d,e): width & height
--drawLine (a,b): startPoint, (c,d): endPoint
--drawCircle a: fill("f") or not, (b,c): orgin point, d: radious (CInt)
--drawDot (a,b): point
--drawGrid (a,b): grid size (CInt,CInt), (c,d): startPosition(upper left): (e,f): width & height
prioDef :: [Definition]
prioDef = [(("cls",[Io]),"cls")
          ,(("clear",[Io]),"clear")
          ,(("a color",[Kaz,Io]),"a color")
          ,(("run",[Io]),"run")
          ,(("a load",[Kaz,Io]),"a load")
          ,(("a waka",[Kaz,Io]),"a waka")
          ,(("a b define",[Moz,Moz,Io]),"a b define")
          ,(("a lineSize",[Kaz,Io]),"a lineSize")
          ,(("a b c d e drawRect",[Moz,Kaz,Kaz,Kaz,Kaz,Io]),"a b c d e drawRect")
          ,(("a b c d drawLine",[Kaz,Kaz,Kaz,Kaz,Io]),"a b c d drawLine")
          ,(("a b c d drawCircle",[Moz,Kaz,Kaz,Kaz,Io]),"a b c d drawCircle")
          ,(("a b drawDot",[Kaz,Kaz,Io]),"a b drawDot")
          ,(("a b c d e f drawGrid",[Kaz,Kaz,Kaz,Kaz,Kaz,Kaz,Io]),"a b c d e f drawGrid")
          ,(("a b c d e f drawImage",[Kaz,Kaz,Kaz,Kaz,Kaz,Moz,Io]),"a b c d e f drawImage")]

speDef :: [String]
speDef = ["(",")","="]

needSpace :: [String]
needSpace = speDef ++ ["x","*","=","::"]

preFunc :: [String] -> String 
preFunc [] = "" 
preFunc ws = 
  case name of
    "pro" -> show $ product args 
    _     -> name 
  where name = last ws
        args = map read (init ws)

