{-# LANGUAGE OverloadedStrings #-}

module Database.Interface (connection, pull) where

import Control.Exception (try, SomeException)
import Control.Exception.Base (Exception)
import Database.PostgreSQL.Simple as PG
import Entry

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "127.0.0.1"
    , connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = "postgres"
    }

connection :: IO (Either SomeException Connection)
connection = try $ PG.connect localPG

pull :: Connection -> IO [Entry]
pull conn = PG.query_ conn "select * from clients"
