module Main where

import Foreign.C.Types
import GUI
import Text.Printf

main :: IO ()
main = exec newWindow
  where
    exec :: Maybe VoidWindow -> IO ()
    exec Nothing = printf "Initialization failure\n"
    exec (Just window) = do_exec window 0

    do_exec :: VoidWindow -> CInt -> IO ()
    do_exec window 0 = do_exec window $ void_gui_exec window
    do_exec window 1 = print $ void_gui_finish window
    do_exec window code = printf "Undexpected code %s" $ show code
