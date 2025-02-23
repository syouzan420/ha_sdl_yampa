module Keys(keyList,Key(..)) where

data Key = KeyUnknown | KeyReturn | KeyEscape | KeyBackspace | KeyTab | KeySpace
          | KeyPlus | KeyComma | KeyMinus | KeyPeriod | KeySlash | Key0
          | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
          | KeyColon | KeySemicolon | KeyLess | KeyEquals | KeyGreater
          | KeyQuestion | KeyAt | KeyLeftBracket | KeyBackslash | KeyRightBracket
          | KeyCaret | KeyUnderscore | KeyBackquote | KeyA | KeyB | KeyC | KeyD
          | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ | KeyK | KeyL | KeyM | KeyN
          | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT | KeyU | KeyV | KeyW | KeyX
          | KeyY | KeyZ | KeyCapsLock | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5
          | KeyF6 | KeyF7 | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
          | KeyPageUp | KeyDelete | KeyEnd | KeyPageDown | KeyRight | KeyLeft
          | KeyDown | KeyUp | KeyLCtrl | KeyLShift | KeyLAlt | KeyLGUI   
          | KeyRCtrl | KeyRShift | KeyRAlt | KeyRGUI
          deriving Eq

keyList :: [Key]
keyList = [KeyUnknown,KeyReturn,KeyEscape,KeyBackspace,KeyTab,KeySpace
          ,KeyPlus,KeyComma,KeyMinus,KeyPeriod,KeySlash,Key0
          ,Key1,Key2,Key3,Key4,Key5,Key6,Key7,Key8,Key9
          ,KeyColon,KeySemicolon,KeyLess,KeyEquals,KeyGreater
          ,KeyQuestion,KeyAt,KeyLeftBracket,KeyBackslash,KeyRightBracket
          ,KeyCaret,KeyUnderscore,KeyBackquote,KeyA,KeyB,KeyC,KeyD
          ,KeyE,KeyF,KeyG,KeyH,KeyI,KeyJ,KeyK,KeyL,KeyM,KeyN
          ,KeyO,KeyP,KeyQ,KeyR,KeyS,KeyT,KeyU,KeyV,KeyW,KeyX
          ,KeyY,KeyZ,KeyCapsLock,KeyF1,KeyF2,KeyF3,KeyF4,KeyF5
          ,KeyF6,KeyF7,KeyF8,KeyF9,KeyF10,KeyF11,KeyF12
          ,KeyPageUp,KeyDelete,KeyEnd,KeyPageDown,KeyRight,KeyLeft
          ,KeyDown,KeyUp,KeyLCtrl,KeyLShift,KeyLAlt,KeyLGUI   
          ,KeyRCtrl,KeyRShift,KeyRAlt,KeyRGUI]

