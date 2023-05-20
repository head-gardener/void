{-# LANGUAGE OverloadedStrings #-}

module Void.CRM.Database (connectInfo, connection, pull, push) where

import Control.Exception (SomeException, try)
import Control.Exception.Base (Exception)
import Control.Monad.Trans.Except
import Data.Functor ((<&>))
import Database.PostgreSQL.Simple as PG
import Debug.Trace (trace)
import Void.CRM.Damage (Damage (..))
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
push c xs = do
  mapM update xs <&> fromIntegral . sum
 where
  -- TODO: execute many

  -- group xs = do_group xs ([], [], [])
  --  where
  --   do_group [] r = r
  --   do_group (Update e : xs) (u, a, r) = do_group xs (Update e : u, a, r)
  --   do_group (Add e : xs) (u, a, r) = do_group xs (u, Add e : a, r)
  --   do_group (Remove i : xs) (u, a, r) = do_group xs (u, a, Remove i : r)

  update (Update (Subscriber uid name phone mou)) =
    PG.execute
      c
      "update clients set name = ?, phone = ?, mou = ? where id = ?"
      (name, phone, mou, uid)
  update (Add (Subscriber uid name phone mou)) =
    PG.execute
      c
      "insert into clients (name, phone, mou, id) values(?, ?, ?, ?)"
      (name, phone, mou, uid)
  update (Remove uid) =
    PG.execute
      c
      "delete from clients where id = ?"
      [uid]
