--
-- Insert Documentation here :-)
--
module Reactive.Banana.OIS (
        InputSystem,
        createInputSystem,
        getKeyE,
        getMouseE,
        startPolling,
        capture,
        KeyCode(..)
) where






import OIS
import OIS.Types hiding (MouseState)

import Reactive.Banana
import Reactive.Banana.Frameworks (
                Frameworks,
                AddHandler,
                fromAddHandler,
                newAddHandler
        )

import Foreign.C.Types (CInt(..))
import Unsafe.Coerce
import Data.List ((\\))
import Control.Monad (
                filterM,
                when
        )
import Control.Concurrent (
                ThreadId,
                threadDelay,
                forkIO
        )






type InputSystem = (
                (Keyboard, (AddHandler KeysPressed, KeysPressed -> IO ())),
                (Mouse,    (AddHandler MouseState, MouseState -> IO ()))
        )
type KeysPressed = [KeyCode]
type MouseState = ()

createInputSystem :: Int -> IO (InputSystem)
createInputSystem hwnd = do
        im <- inputManager_createInputSystem_size_t hwnd
        -- get mouse and keyboard objects
        -- unsafeCoerce instead of static_cast
        mouse <- unsafeCoerce $ inputManager_createInputObject im OISMouse True ""
        keyboard <- unsafeCoerce $ inputManager_createInputObject im OISKeyboard True ""
        -- create the addhandlers
        mouseNewAddHandler <- newAddHandler
        keyboardNewAddHandler <- newAddHandler
        -- done, package into a InputSystem
        return ((keyboard, keyboardNewAddHandler), (mouse, mouseNewAddHandler))

getKeyE :: Frameworks t => InputSystem -> Moment t (Event t KeysPressed)
getKeyE = fromAddHandler . getKeyboardAddHandler

getMouseE :: Frameworks t => InputSystem -> Moment t (Event t MouseState)
getMouseE = fromAddHandler . getMouseAddHandler

startPolling :: InputSystem -> Int -> IO (ThreadId)
startPolling is pollDelay = do
        -- capture input devices
        capture is
        -- sleep thread
        threadDelay pollDelay
        -- loop
        startPolling is pollDelay
        
poll :: InputSystem -> IO ()
poll is = do
        -- poll keyboard
        -- at the moment only poll a few keys
        keysDown <- getKeysDown is
        let keyPressed =  keysDown
        fireKeyboadEvent is keyPressed
        
        -- poll mouse
        -- TODO
        

getKeysDown :: InputSystem -> IO (KeysPressed)
getKeysDown is = filterM (isKeyDown is) allKeyCodes

--
-- some direct polling functions
--

-- call the capture function of all input Objects, and poll for changes
capture :: InputSystem -> IO ()
capture is@((kb,_), (ms,_)) = do
        object_capture $ toObject kb 
        object_capture $ toObject ms
        poll is 

-- Check if a key is down. Note that you should call capture before using isKeyDown
isKeyDown :: InputSystem -> KeyCode -> IO (Bool)
isKeyDown ((kb,_), _) = keyboard_isKeyDown kb

-- Check if a mouse button down. Note that you should call capture before using isKeyDown
isMouseButtonDown :: InputSystem -> KeyCode -> IO (Bool)
isMouseButtonDown ((kb,_), _) = keyboard_isKeyDown kb

--
-- some accessor functions
--

getKeyboardAddHandler :: InputSystem -> AddHandler KeysPressed
getKeyboardAddHandler ((_, (kbah,_)), _) = kbah

getMouseAddHandler :: InputSystem -> AddHandler MouseState
getMouseAddHandler (_, (_, (msah,_))) = msah
        
fireKeyboadEvent :: InputSystem -> KeysPressed -> IO ()
fireKeyboadEvent ((_, (_,kbcb)), _) = kbcb

fireMouseEvent :: InputSystem -> MouseState -> IO ()
fireMouseEvent (_, (_, (_,mscb))) = mscb

-- modify the KeyCodes to be more usable
allKeyCodes :: [KeyCode]
allKeyCodes = [KC_UNASSIGNED,KC_ESCAPE,KC_1,KC_2,KC_3,KC_4,KC_5,KC_6,KC_7,KC_8,KC_9,KC_0,KC_MINUS,KC_EQUALS,KC_BACK,KC_TAB,KC_Q,KC_W,KC_E,KC_R,KC_T,KC_Y,KC_U,KC_I,KC_O,KC_P,KC_LBRACKET,KC_RBRACKET,KC_RETURN,KC_LCONTROL,KC_A,KC_S,KC_D,KC_F,KC_G,KC_H,KC_J,KC_K,KC_L,KC_SEMICOLON,KC_APOSTROPHE,KC_GRAVE,KC_LSHIFT,KC_BACKSLASH,KC_Z,KC_X,KC_C,KC_V,KC_B,KC_N,KC_M,KC_COMMA,KC_PERIOD,KC_SLASH,KC_RSHIFT,KC_MULTIPLY,KC_LMENU,KC_SPACE,KC_CAPITAL,KC_F1,KC_F2,KC_F3,KC_F4,KC_F5,KC_F6,KC_F7,KC_F8,KC_F9,KC_F10,KC_NUMLOCK,KC_SCROLL,KC_NUMPAD7,KC_NUMPAD8,KC_NUMPAD9,KC_SUBTRACT,KC_NUMPAD4,KC_NUMPAD5,KC_NUMPAD6,KC_ADD,KC_NUMPAD1,KC_NUMPAD2,KC_NUMPAD3,KC_NUMPAD0,KC_DECIMAL,KC_OEM_102,KC_F11,KC_F12,KC_F13,KC_F14,KC_F15,KC_KANA,KC_ABNT_C1,KC_CONVERT,KC_NOCONVERT,KC_YEN,KC_ABNT_C2,KC_NUMPADEQUALS,KC_PREVTRACK,KC_AT,KC_COLON,KC_UNDERLINE,KC_KANJI,KC_STOP,KC_AX,KC_UNLABELED,KC_NEXTTRACK,KC_NUMPADENTER,KC_RCONTROL,KC_MUTE,KC_CALCULATOR,KC_PLAYPAUSE,KC_MEDIASTOP,KC_VOLUMEDOWN,KC_VOLUMEUP,KC_WEBHOME,KC_NUMPADCOMMA,KC_DIVIDE,KC_SYSRQ,KC_RMENU,KC_PAUSE,KC_HOME,KC_UP,KC_PGUP,KC_LEFT,KC_RIGHT,KC_END,KC_DOWN,KC_PGDOWN,KC_INSERT,KC_DELETE,KC_LWIN,KC_RWIN,KC_APPS,KC_POWER,KC_SLEEP,KC_WAKE,KC_WEBSEARCH,KC_WEBFAVORITES,KC_WEBREFRESH,KC_WEBSTOP,KC_WEBFORWARD,KC_WEBBACK,KC_MYCOMPUTER,KC_MAIL,KC_MEDIASELECT]

instance Enum KeyCode where
        toEnum = cintToKeyCode . CInt . fromIntegral
        fromEnum = fromIntegral . keyCodeToCInt
instance Eq KeyCode where
        a == b = (fromEnum a) == (fromEnum b)
instance Show KeyCode where
        show = show . fromEnum
-- TODO make KeyCodes instance of Enum and Bounded






