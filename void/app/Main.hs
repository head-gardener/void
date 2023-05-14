module Main where

import Database.Interface as DB
import Entry
import Foreign.C.Types
import qualified GUI as G
import Text.Printf

main :: IO ()
main =
  G.withNewInstance $ \w -> do
    fill w
    exec w
 where
  pull :: G.VoidInstance -> IO ()
  pull w = do
    conn <- DB.connection
    case conn of
      Left e -> putStrLn $ "Connection error: " ++ show e
      Right conn -> DB.pull conn >>= (G.drop w `seq` G.push w)

  push :: G.VoidInstance -> IO ()
  push w = do
    conn <- DB.connection
    case conn of
      Left e -> putStrLn $ "Connection error: " ++ show e
      Right conn -> G.pull w >>= DB.push conn

  fill :: G.VoidInstance -> IO ()
  fill w =
    G.push
      w
      [ Entry "Vovan" "123"
      , Entry "Ivan" "228"
      ]

  exec :: G.VoidInstance -> IO ()
  exec window = do_exec window 0

  do_exec :: G.VoidInstance -> CInt -> IO ()
  do_exec w 0 = do_exec w $ G.wait w
  do_exec w 3 = push w >> do_exec w (G.wait w)
  do_exec w 2 = pull w >> do_exec w (G.wait w)
  do_exec w 1 = return ()
  do_exec w code = putStrLn ("Unexpected code " ++ show code) >> do_exec w (G.wait w)
