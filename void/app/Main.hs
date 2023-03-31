module Main where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GUI
import Text.Printf

main :: IO ()
main = exec window 1
  where
    window = void_gui_init 1000

    exec :: VoidWindow -> CInt -> IO ()
    -- TODO: exec nullWindow code = print "Window freed unexpectedly"
    exec window 0 = print $ void_gui_finish window
    exec window 1 = exec window $ void_gui_exec window
    exec window code = printf "undexpected code %s" $ show code
