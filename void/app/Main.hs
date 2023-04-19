module Main where

import Data
import Database.Interface as DB
import Foreign.C.Types
import GUI
import Text.Printf

main :: IO ()
main =
  do
    e <- pull
    withNewWindow $ \w -> do
      addToWindow e w
      exec w
  where
    pull :: IO [Entry]
    pull =
      let x = do
            conn <- DB.connection
            DB.pull conn

          unwrap :: Maybe (IO [Entry]) -> IO [Entry]
          unwrap (Just x) = x
          unwrap Nothing = error "Pull failure"
       in unwrap x

    exec :: VoidWindow -> IO ()
    exec window = do_exec window 0

    do_exec :: VoidWindow -> CInt -> IO ()
    do_exec window 0 = do_exec window $ waitWindow window
    do_exec window 1 = print $ waitWindow window
    do_exec window code = putStrLn $ "Unexpected code" ++ show code
