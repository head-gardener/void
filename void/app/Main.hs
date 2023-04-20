module Main where

import Data
import Database.Interface as DB
import Foreign.C.Types
import qualified GUI as G
import Text.Printf

main :: IO ()
main =
  G.withNewWindow $ \w -> do
    exec w
  where
    pull :: G.VoidWindow -> IO ()
    pull w =
      let d = do
            conn <- DB.connection
            DB.pull conn

          update (Just x) = do
            e <- x
            G.drop w `seq` G.push w e
          update Nothing = putStrLn "Pull failure"
       in update d

    exec :: G.VoidWindow -> IO ()
    exec window = do_exec window 0

    do_exec :: G.VoidWindow -> CInt -> IO ()
    do_exec w 0 = do_exec w $ G.wait w
    do_exec w 2 = do
      pull w
      do_exec w 0
    do_exec w 1 = return ()
    do_exec w code = do
      putStrLn $ "Unexpected code " ++ show code
      do_exec w 0
