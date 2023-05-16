module Main where

import Control.Exception
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (Except, ExceptT (..), runExceptT)
import Control.Monad.Trans.State.Lazy
import Database.Interface as DB
import Database.PostgreSQL.Simple (ConnectInfo, Connection)
import Entry
import Foreign.C.Types
import qualified GUI as G
import Text.Printf

initialState :: G.VoidInstance -> ConnectInfo -> WindowState
initialState w i = (w, DB.connection, False, i)

type WindowState = (G.VoidInstance, ExceptT SomeException IO Connection, Bool, ConnectInfo)

main :: IO ()
main =
  G.withNewInstance $ \w -> do
    fill w
    exec w
 where
  tryWithSyncedConn :: (Connection -> IO ()) -> StateT WindowState IO ()
  tryWithSyncedConn f = do
    (w, _, s, _) <- get
    if s
      then tryWithConnection f
      else liftIO $ G.putStatus w "Not synced!"

  tryWithConnection :: (Connection -> IO ()) -> StateT WindowState IO ()
  tryWithConnection f = do
    (w, c, s, i) <- get
    liftIO (runExceptT c >>= handleConnection (w, c, s, i) f) >>= put
   where
    handleConnection :: WindowState -> (Connection -> IO ()) -> Either SomeException Connection -> IO WindowState
    handleConnection (w, c, _, i) f (Left e) = G.putStatus w "Connection error!" >> return (initialState w i)
    handleConnection (w, _, s, i) f (Right c) = liftIO (f c) >> return (w, ExceptT $ return $ Right c, True, i)

  pull :: StateT WindowState IO ()
  pull = do
    (w, c, _, _) <- get
    tryWithConnection
      ( DB.pull
          >=> (\es -> G.drop w `seq` G.putStatus w "Pull successful" >> G.push w es)
      )

  push :: StateT WindowState IO ()
  push = do
    (w, c, _, _) <- get
    tryWithSyncedConn (\c -> G.pull w >>= DB.push c >>= prettyPrint w)
   where
    prettyPrint :: G.VoidInstance -> Int -> IO ()
    prettyPrint w n = G.putStatus w $ "Changes pushed: " ++ show n

  fill :: G.VoidInstance -> IO ()
  fill w =
    G.push
      w
      [ Entry 0 "Vovan" "123"
      , Entry 1 "Ivan" "228"
      ]

  exec :: G.VoidInstance -> IO ()
  exec w = evalStateT do_exec $ initialState w DB.connectInfo

  do_exec :: StateT WindowState IO ()
  do_exec = do
    (w, _, _, _) <- get
    case G.wait w of
      1 -> return ()
      2 -> pull >> do_exec
      3 -> push >> do_exec
      _ -> do_exec
