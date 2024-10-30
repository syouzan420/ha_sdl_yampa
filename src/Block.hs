module Block where

import Mana.Mana (Mn(..),Yo(..),Df(..),DefList,setDf)
import General (getIndex, removeIndex, toGrid)
import Data.Bifunctor (bimap)
import Data.List (nub)

type Name = String
type Value = String
data BCell = Core Mn | Hand Mn | Empt deriving (Eq, Show)
type BGrid = [[BCell]]
type GSize = (Int,Int)
type GPos = (Int,Int)
type RPos = (Int,Int)
type Block = (Mn,[RPos])

makeNewGrid :: GSize -> BGrid
makeNewGrid (_,0) = []
makeNewGrid (x,y) = makeXGrid x:makeNewGrid (x,y-1)

makeXGrid :: Int -> [BCell] 
makeXGrid 0 = [] 
makeXGrid x = Empt:makeXGrid (x-1)

getGridSize :: BGrid -> GSize
getGridSize gr = if null gr then (0,0) else (length$head gr,length gr)

getCenter :: GSize -> GPos
getCenter (a,b) = (a `div` 2, b `div` 2)

putFirstBlock :: DefList -> BGrid -> Block -> BGrid
putFirstBlock dfl gr b = let gsize = getGridSize gr 
                             cpos = getCenter gsize
                          in putBlock dfl gr cpos b

getCell :: BGrid -> GPos -> BCell
getCell gr (p,q) = (gr!!q)!!p

isOnGrid :: GSize -> GPos -> [RPos] -> Bool
isOnGrid _ _ [] = True 
isOnGrid (a,b) (p,q) ((dx,dy):xs)  
  | p+dx >= 0 && p+dx < a && q+dy >= 0 && q+dy < b = isOnGrid (a,b) (p,q) xs
  | otherwise = False

isOffMana :: BGrid -> GPos -> [RPos] -> Bool 
isOffMana _ _ [] = True
isOffMana gr (p,q) ((dx,dy):xs) 
  | cell==Empt = isOffMana gr (p,q) xs
  | otherwise = False
  where cell = getCell gr (p+dx,q+dy) 

isBeHand :: BGrid -> GPos -> Mn -> [RPos] -> Bool
isBeHand gr pos (Mn _ y) [] = let cell = getCell gr pos
                               in case cell of Hand (Mn _ yo) -> y == yo; _ -> False
isBeHand _ _ _ _ = False

canPutBlock :: BGrid -> GPos -> Block -> Bool 
canPutBlock gr pos (_,rps) = let gsize = getGridSize gr
                                 iOnGrid = isOnGrid gsize pos rps
                              in iOnGrid && isOffMana gr pos rps

blockToCells :: DefList -> Block -> [BCell]
blockToCells dfl (mn@(Mn t y),_) 
  | y==Def = let (Df _ dp dy _) = setDf dfl t
                 pList = words dp
                 dind = getIndex t pList
                 hyList = removeIndex dind dy
              in Core mn:map (Hand . Mn "") hyList
  | otherwise = [Core mn]

handPositions :: GPos -> [RPos] -> [GPos]
handPositions (p,q) = map (bimap (p +) (q +)) 

putBlock :: DefList -> BGrid -> GPos -> Block -> BGrid
putBlock dfl gr gpos b@(_,rps) = let cells = blockToCells dfl b
                                     poss = gpos:handPositions gpos rps
                                  in foldl (\acc (pos,cell) -> toGrid acc pos cell) gr (zip poss cells)

isBlock :: BGrid -> GPos -> Bool
isBlock gr pos = getCell gr pos /= Empt

putablePos :: BGrid -> GPos -> [GPos]
putablePos gr = putablePos' (getGridSize gr) (-1,-1) gr

putablePos' :: GSize -> GPos -> BGrid -> GPos -> [GPos]
putablePos' (sx,sy) ppos gr (p,q) =
  let kposs = filter (/=ppos) $ 
        filter (\(x,y) -> x >= 0 && x < sx && y >= 0 && y < sy) [(p+1,q),(p,q-1),(p-1,q),(p,q+1)]
   in nub $ foldl (\acc kp -> if isBlock gr kp then acc ++ putablePos' (sx,sy) (p,q) gr kp 
                                               else acc ++ [kp]) [] kposs

rotateBlock :: Bool -> Block -> Block
rotateBlock d (mn,rps) = (mn,map (rotatePos d) rps)

rotatePos :: Bool -> RPos -> RPos
rotatePos d (x,y) = let k = if d then 1 else -1 in (k*(-y),k*x)


