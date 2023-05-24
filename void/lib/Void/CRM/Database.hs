{-# LANGUAGE OverloadedStrings #-}

module Void.CRM.Database (connectInfo, connection, pull, push) where

import Control.Exception (SomeException, try)
import Control.Exception.Base (Exception)
import Control.Monad.Trans.Except
import Data.Functor ((<&>))
import Data.Int
import Database.PostgreSQL.Simple as PG
import Debug.Trace (trace, traceShow)
import Void.CRM.Damage (Damage (..), group)
import Void.CRM.Subscriber

connectInfo :: ConnectInfo
connectInfo =
  defaultConnectInfo
    { connectHost = "127.0.0.1"
    , connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = "postgres"
    }

connection :: ExceptT SomeException IO Connection
connection = ExceptT $ try $ PG.connect connectInfo

pull :: Connection -> IO [Subscriber]
pull conn = PG.query_ conn "select * from clients"

push :: Connection -> [Damage] -> IO Int
push c ds = do
  let (us, is, rs) = group ds
  u <- mapM (update . castSubscriber) us <&> sum
  i <- insert (map castSubscriber is)
  r <- remove (map castUID rs)
  return (u + i + r) <&> fromIntegral
 where
  castSubscriber (Subscriber uid name phone mou plan) = (name, phone, mou, uid)
  castUID uid = [uid]

  update =
    PG.execute
      c
      "update clients set name = ?, phone = ?, mou = ? where id = ?"
  insert =
    PG.executeMany
      c
      "insert into clients (name, phone, mou, id) values(?, ?, ?, ?)"
  remove =
    PG.executeMany
      c
      "delete from clients where id = ?"
