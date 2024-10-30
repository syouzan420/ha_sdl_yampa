module Game.WkLib (cosList, shiftList) where

type Len = Int      --length
type Amp = Int      --amplitude
type Frq = Int      --frequency
type Count = Int    --count(time)
type Shift = Int    --shift degrees (right shift (plus) left shift (minus))

cosList :: Len -> Amp -> Frq -> Count -> [Int]
cosList l a f c = map (\x -> round (fromIntegral a * cos ((2*pi*fromIntegral f/fromIntegral l)*(fromIntegral x-fromIntegral c))::Double)) [0..(l-1)] 

shiftList :: a -> [a] -> Shift -> [a]
shiftList _ [] _ = []
shiftList sp lst s
  | absS > lng = lst
  | otherwise = if s>0 then replicate s sp ++ take (lng-s) lst
                       else drop absS lst ++ replicate absS sp 
       where lng = length lst
             absS = abs s
