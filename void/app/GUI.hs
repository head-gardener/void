{-# LANGUAGE ForeignFunctionInterface #-}

module GUI where

import Foreign
import Foreign.C.Types

foreign import ccall "voidgui.h void_gui_init"
  void_gui_init :: VoidWindow

foreign import ccall "voidgui.h void_gui_exec"
  void_gui_exec :: VoidWindow -> CInt

foreign import ccall "voidgui.h void_gui_finish"
  void_gui_finish :: VoidWindow -> CInt

newtype VoidWindow = VoidWindow (Ptr VoidWindow) deriving (Eq)

nullWindow :: VoidWindow
nullWindow = VoidWindow nullPtr

newWindow :: Maybe VoidWindow
newWindow = checkWindow void_gui_init

checkWindow :: VoidWindow -> Maybe VoidWindow
checkWindow x
  | x == nullWindow = Nothing
  | otherwise = Just x
