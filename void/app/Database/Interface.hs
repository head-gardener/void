{-# LANGUAGE OverloadedStrings #-}

module Database.Interface (connection, pull) where

import Entry
import Database.PostgreSQL.Simple as PG

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "postgres",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

connection :: Maybe (IO Connection)
connection =
  let conn = PG.connect localPG
   in Just conn

pull :: IO Connection -> Maybe (IO [Entry])
pull conn =
  let q = "select * from clients"
      res = do
        c <- conn
        PG.query_ c q :: IO [Entry]
   in Just res
