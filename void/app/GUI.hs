{-# LANGUAGE ForeignFunctionInterface #-}

module GUI (addToWindow, withNewWindow, waitWindow, VoidWindow) where

import Data
import Foreign
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "voidgui.h void_gui_init"
  void_gui_init :: VoidWindow

foreign import ccall "voidgui.h void_gui_exec"
  void_gui_exec :: VoidWindow -> CInt

foreign import ccall "voidgui.h void_gui_finish"
  void_gui_finish :: VoidWindow -> CInt

foreign import ccall "voidgui.h void_gui_add"
  void_gui_add :: Ptr CWchar -> Ptr CWchar -> VoidWindow -> CInt

newtype VoidWindow = VoidWindow (Ptr VoidWindow) deriving (Eq)

nullWindow :: VoidWindow
nullWindow = VoidWindow nullPtr

withNewWindow :: (VoidWindow -> IO ()) -> IO ()
withNewWindow f = 
  let 
    exec (Just w) = do 
      f w
      void_gui_finish w `seq` return ()
    exec Nothing = putStrLn "Initialization failure"
  in do 
    exec newWindow

newWindow :: Maybe VoidWindow
newWindow = void_gui_init `seq` checkWindow void_gui_init
  where
    checkWindow :: VoidWindow -> Maybe VoidWindow
    checkWindow x
      | x == nullWindow = Nothing
      | otherwise = Just x

waitWindow :: VoidWindow -> CInt
waitWindow = void_gui_exec

-- not super efficient cause of marshalling and all.
addToWindow :: [Entry] -> VoidWindow -> IO ()
addToWindow (e : es) window =
  withCWString (name e) $ \cn ->
    withCWString (phone e) $ \ct -> do
      let code = void_gui_add cn ct window
      code `seq` addToWindow es window
addToWindow [] window =
  return ()
