module MyApp(appMain) where

import qualified Control.Monad.State.Strict as S
import FRP.Yampa (identity,reactimate)
import Connector(stopTextInput,myLoad,Loaded(..)
                ,withMyInit,withMyVideo,getImageSize)
import MyEvent (inputEvent,initInput)
import MyOutput (myOut)
import MyData (initState,initActive,initAttr,initJumping
              ,State(..),Active(..),Attr(..),Jumping(..))

appMain :: IO ()
appMain =
  withMyInit $ do
    Loaded fonts sur text (fpos,tpos) dots jumps <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        imgSizes <- getImageSize itexs 
        let newActive = initActive{tex=text,dts=dots,fps=fpos,tps=tpos}
            newAttr = initAttr{jmp=initJumping{jps=jumps}}
            newState = initState{isz=imgSizes,act=newActive,atr=newAttr} 
        stopTextInput
        S.runStateT (reactimate 
                        initInput 
                        inputEvent 
                        (myOut renderer fonts itexs)
                        identity 
                    ) newState

