module SDLDriver(myLoad,Loaded(..),withMyInit,withMyVideo
                ,getImageSize,myInput,InpRes(..),Rect(..)
                ,startTextInput,stopTextInput
                ,Font,Renderer,Texture,delay,myDraw,toKey) where

import MySDL.MyLoad (myLoad,Loaded(..))
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo,getImageSize)

import MySDL.MyInput (myInput,InpRes(..))
import SDL.Raw.Types (Rect(..))
import SDL.Input.Keyboard (startTextInput,stopTextInput)

import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import MySDL.MyDraw (myDraw)

import SDL.Input.Keyboard.Codes
import Keys(keyList,Key(KeyUnknown))
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

asocList :: Map.Map Keycode Key
asocList = Map.fromList (zip keycodeList keyList)

toKey :: Keycode -> Key
toKey kc = fromMaybe KeyUnknown $ Map.lookup kc asocList 

keycodeList :: [Keycode]
keycodeList = [KeycodeUnknown,KeycodeReturn,KeycodeEscape,KeycodeBackspace
              ,KeycodeTab,KeycodeSpace
              ,KeycodePlus,KeycodeComma,KeycodeMinus,KeycodePeriod,KeycodeSlash
              ,Keycode0,Keycode1,Keycode2,Keycode3,Keycode4,Keycode5,Keycode6
              ,Keycode7,Keycode8,Keycode9
              ,KeycodeColon,KeycodeSemicolon,KeycodeLess,KeycodeEquals
              ,KeycodeGreater
              ,KeycodeQuestion,KeycodeAt,KeycodeLeftBracket,KeycodeBackslash
              ,KeycodeRightBracket
              ,KeycodeCaret,KeycodeUnderscore,KeycodeBackquote,KeycodeA,KeycodeB
              ,KeycodeC,KeycodeD
              ,KeycodeE,KeycodeF,KeycodeG,KeycodeH,KeycodeI,KeycodeJ,KeycodeK
              ,KeycodeL,KeycodeM,KeycodeN
              ,KeycodeO,KeycodeP,KeycodeQ,KeycodeR,KeycodeS,KeycodeT,KeycodeU
              ,KeycodeV,KeycodeW,KeycodeX
              ,KeycodeY,KeycodeZ,KeycodeCapsLock,KeycodeF1,KeycodeF2,KeycodeF3
              ,KeycodeF4,KeycodeF5
              ,KeycodeF6,KeycodeF7,KeycodeF8,KeycodeF9,KeycodeF10,KeycodeF11
              ,KeycodeF12
              ,KeycodePageUp,KeycodeDelete,KeycodeEnd,KeycodePageDown,KeycodeRight
              ,KeycodeLeft
              ,KeycodeDown,KeycodeUp,KeycodeLCtrl,KeycodeLShift,KeycodeLAlt
              ,KeycodeLGUI   
              ,KeycodeRCtrl,KeycodeRShift,KeycodeRAlt,KeycodeRGUI]
