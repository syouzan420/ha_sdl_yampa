module MySDL.MyInput (myInput,InpRes(..)) where

import SDL.Event (EventPayload(KeyboardEvent,TextInputEvent,TextEditingEvent
                              ,MouseButtonEvent,MouseMotionEvent)
                 ,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents
                 ,TextInputEventData(textInputEventText),TextEditingEventData(textEditingEventText)
                 ,MouseButtonEventData(mouseButtonEventMotion
                                ,mouseButtonEventPos,mouseButtonEventButton)
                 ,MouseMotionEventData(mouseMotionEventState,mouseMotionEventPos)
                 ,MouseButton(ButtonLeft))
import SDL.Input.Keyboard (Keysym(keysymKeycode,keysymModifier),KeyModifier(..)
                          ,getModState)
import SDL.Input.Keyboard.Codes
import SDL.Vect(Point(P),V2(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Data.List(find)
import Foreign.C.Types(CInt)
import MyData(Modif(..))

data InpRes = InpRes !Keycode !Modif !T.Text !(V2 CInt,V2 CInt) !(Bool,Bool) !Bool !Bool 
  
myInput :: MonadIO m => m (Maybe InpRes) 
myInput = do
  evts <- pollEvents
  case evts of
    [] -> return Nothing
    events -> do
      mds <- getModState
      let kcmd event  =  
            case eventPayload event of 
              KeyboardEvent keyboardEvent ->
                case keyboardEventKeyMotion keyboardEvent of
                  Pressed -> let kbeSim = keyboardEventKeysym keyboardEvent
                              in (keysymKeycode kbeSim,keysymModifier kbeSim)
                  _other  -> (KeycodeUnknown,mds)
              _other -> (KeycodeUnknown,mds)
          kir event = 
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                case keyboardEventKeyMotion keyboardEvent of
                  Released -> True
                  _other   -> False
              _other                      -> False
          getItx event = 
            case eventPayload event of
              TextInputEvent textInputEvent
                      -> (textInputEventText textInputEvent,False)
              TextEditingEvent textEditingEvent 
                      -> (textEditingEventText textEditingEvent,True)
              _other      -> (T.empty,False)
          mbtn event = 
            case eventPayload event of
              MouseButtonEvent mouseButtonEvent ->
                if mouseButtonEventMotion mouseButtonEvent == Pressed
                    then mouseButtonEventPos mouseButtonEvent
                    else P (V2 (-1) (-1))
--              MouseMotionEvent mouseMotionEvent ->
--                                  mouseMotionEventPos mouseMotionEvent
              MouseMotionEvent mouseMotionEvent ->
                if mouseMotionEventState mouseMotionEvent == [ButtonLeft]
                    then mouseMotionEventPos mouseMotionEvent
                    else P (V2 (-1) (-1))
              _other                            -> P (V2 (-1) (-1))

          motn event = 
            case eventPayload event of
              MouseMotionEvent mouseMotionEvent ->
                            mouseMotionEventPos mouseMotionEvent
              _other                            -> P (V2 (-1) (-1))

          mmcl event =
            case eventPayload event of
              MouseButtonEvent mouseButtonEvent ->
                mouseButtonEventButton mouseButtonEvent == ButtonLeft
          --      mouseButtonEventMotion mouseButtonEvent == Pressed 
              _other -> False

          mmtn event = 
            case eventPayload event of
              MouseMotionEvent mouseMotionEvent ->
                mouseMotionEventState mouseMotionEvent == [ButtonLeft]
              _other                            -> False 
                     
          ir =  not (null events) && last (map kir events)
          (kc,md) = fromMaybe (KeycodeUnknown,mds) $ find (/=(KeycodeUnknown,mds)) (reverse (map kcmd events)) 
          (itx,ised) = fromMaybe (T.empty,False) $ find (/=(T.empty,False)) $ 
                                  filter (/=(T.empty,True)) (map getItx events) 
          cPos = fromMaybe (P (V2 (-1) (-1))) $ find (/=P (V2 (-1) (-1))) (map mbtn events)
          mPos = fromMaybe (P (V2 (-1) (-1))) $ find (/=P (V2 (-1) (-1))) (map motn events)
          ismc = fromMaybe False $ Just (not (null events) && head (map mmtn events))
          iscl = fromMaybe False $ Just (not (null events) && head (map mmcl events))
          mdres
            | keyModifierLeftShift md || keyModifierRightShift md = Shf 
            | keyModifierLeftCtrl md || keyModifierRightCtrl md = Ctr 
            | keyModifierLeftAlt md || keyModifierRightAlt md = Alt 
            | otherwise = Non 

          mps = let (P (V2 px py)) = cPos in V2 (fromIntegral px) (fromIntegral py)

          mps0 = let (P (V2 px py)) = mPos in V2 (fromIntegral px) (fromIntegral py)

      return $ Just (InpRes kc mdres itx (mps,mps0) (ismc,iscl) ised ir) 
 
