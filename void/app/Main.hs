module Main where

import Control.Exception (SomeException)
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (Except, ExceptT (..), runExceptT)
import Control.Monad.Trans.State.Lazy
import Database.PostgreSQL.Simple (ConnectInfo, Connection)
import Foreign.C.Types
import Text.Printf
import qualified Void.CRM.Database as DB
import qualified Void.CRM.GUI as G
import Void.CRM.Plan
import Void.CRM.Subscriber

initialState :: G.VoidInstance -> ConnectInfo -> WindowState
initialState w i = (w, DB.connection, False, i)

type WindowState = (G.VoidInstance, ExceptT SomeException IO Connection, Bool, ConnectInfo)

main :: IO ()
main =
  G.withNewInstance $ \w -> do
    fill w
    exec w

fill :: G.VoidInstance -> IO ()
fill w =
  G.push
    w
    0
    [ Subscriber 0 "Vovan" "123" 300 100
    , Subscriber 1 "Ivan" "228" 523 101
    ]
    >> G.push
      w
      1
      [ Plan 100 "Basic" 10 300
      , Plan 101 "Gold" 15 523
      ]

requests :: G.VoidInstance -> [CInt]
requests w = G.wait w : requests w

exec :: G.VoidInstance -> IO ()
exec w = do
  evalStateT
    (mapM_ handle $ takeWhile (/= 1) $ requests w)
    $ initialState w DB.connectInfo

pull :: StateT WindowState IO ()
pull = do
  (w, c, _, _) <- get
  tryWithConnection
    ( \c -> do
        subs <- (fst . DB.pull) c
        plans <- (snd . DB.pull) c
        G.drop w `seq` return ()
        G.push w 0 subs
        G.push w 1 plans
        G.putStatus w "Pull successful"
    )

push :: StateT WindowState IO ()
push = do
  (w, c, _, _) <- get
  tryWithSyncedConn (\c -> G.pull w >>= DB.push c >>= prettyPrint w)
 where
  prettyPrint w n = G.putStatus w $ "Rows affected: " ++ show n

handle :: CInt -> StateT WindowState IO ()
handle 2 = pull
handle 3 = push
handle c = liftIO $ putStrLn $ "unexpected code: " ++ show c

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
  handleConnection (w, c, _, i) f (Left e) =
    G.putStatus w "Connection error!" >> return (initialState w i)
  handleConnection (w, _, s, i) f (Right c) =
    liftIO (f c) >> return (w, ExceptT $ return $ Right c, True, i)
