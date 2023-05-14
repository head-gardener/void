module Main where

import Database.Interface as DB
import Entry
import Foreign.C.Types
import qualified GUI as G
import Text.Printf

main :: IO ()
main =
  G.withNewWindow $ \w -> do
    fill w
    exec w
 where
  pull :: G.VoidWindow -> IO ()
  pull w = do
    conn <- DB.connection
    case conn of
      Left e -> putStrLn $ "Connection error: " ++ show e
      Right conn -> DB.pull conn >>= (G.drop `seq` G.push w)

  fill :: G.VoidWindow -> IO ()
  fill w =
    G.push
      w
      [ Entry "Vovan" "123"
      , Entry "Ivan" "228"
      ]

  exec :: G.VoidWindow -> IO ()
  exec window = do_exec window 0

  do_exec :: G.VoidWindow -> CInt -> IO ()
  do_exec w 0 = do_exec w $ G.wait w
  do_exec w 2 = do
    pull w
    do_exec w 0
  do_exec w 1 = do
    s <- G.pull w
    print s
  do_exec w code = do
    putStrLn $ "Unexpected code " ++ show code
    do_exec w 0
