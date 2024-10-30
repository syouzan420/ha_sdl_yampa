module Mana.MyTree (Elm(..), L, R(..), numR, mtR, ltR, showF, addElem) where

import Data.Tree 
import Data.List (intercalate)

type L = Int
data R = Ri Int | Rc deriving (Eq, Show)
data Elm a = El a L R 

numR :: R -> Int
numR (Ri i) = i
numR Rc = -1

mtR :: R -> Int -> Bool
mtR Rc _ = False
mtR (Ri i) n = i > n

ltR :: R -> Int -> Bool
ltR Rc _ = False
ltR (Ri i) n = i < n

showT :: Show a => Tree a -> String 
showT (Node x []) = show x
showT (Node x tr) = "["++show x++", "++tail (showF tr)

showF :: Show a => Forest a -> String 
showF [] = ""
showF tr = "[" ++ intercalate ", " (map showT tr) ++ "]"

isLastLeaf :: Int -> Forest a -> Bool
isLastLeaf 0 fo = null fo || (let (Node _ sf) = last fo in null sf)  
isLastLeaf i fo = null fo || (let (Node _ sf) = last fo in isLastLeaf (i-1) sf)

addElem :: Elm a -> Forest a -> Forest a 
addElem (El mn _ _) [] = [Node mn []]
addElem (El mn l r) fo
  | lng >= l && l /= 0
          = let (h,(Node s sf):t) = splitAt (lng - abs l) fo
                newNode = if l==1 || l==(-1) || null sf then Node s (t ++ addElem (El mn l r) sf)
                                             else let (Node s' _) = head sf
                                                   in Node s (Node s' (tail sf):t ++ [Node mn []])
                --newNode = Node s (t ++ addElem (El mn l r) sf)
             in h ++ [newNode]
  | otherwise 
          = let (it,lt) = (init fo,last fo)
                Node x subf = lt 
                isAddTree = if mtR r 0 || r == Rc then null subf else isLastLeaf (abs (numR r)) subf
             in if isAddTree then fo ++ [Node mn []]
                             else it ++ [Node x (addElem (El mn l r) subf)]
  where lng = length fo

