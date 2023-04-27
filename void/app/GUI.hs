{-# LANGUAGE ForeignFunctionInterface #-}

-- module GUI (push, GUI.drop, withNewWindow, wait, VoidWindow) where
module GUI where

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
  void_gui_add :: VoidWindow -> CInt

-- foreign import ccall "voidgui.h void_gui_add"
--   void_gui_add :: Ptr CWchar -> Ptr CWchar -> VoidWindow -> CInt

-- foreign import ccall "voidgui.h void_gui_drop"
--   void_gui_drop :: VoidWindow -> CInt

-- compiler might decide to free this object at any point which would be a
-- real nuisance
newtype VoidWindow = VoidWindow (Ptr VoidWindow) deriving (Eq)

nullWindow :: VoidWindow
nullWindow = VoidWindow nullPtr

withNewWindow :: (VoidWindow -> IO ()) -> IO ()
withNewWindow f =
  let exec (Just w) = do
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

wait :: VoidWindow -> CInt
wait = void_gui_exec

add :: VoidWindow -> CInt
add = void_gui_add

-- -- not super efficient cause of marshalling and all.
-- push :: VoidWindow -> [Entry] -> IO ()
-- push window (e : es) =
--   withCWString (name e) $ \cn ->
--     withCWString (phone e) $ \ct -> do
--       let code = void_gui_add cn ct window
--       code `seq` push window es
-- push window [] =
--   return ()

-- drop :: VoidWindow -> CInt
-- drop = void_gui_drop
