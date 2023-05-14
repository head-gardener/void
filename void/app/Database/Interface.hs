{-# LANGUAGE OverloadedStrings #-}

module Database.Interface (connection, pull, push) where

import Control.Exception (SomeException, try)
import Control.Exception.Base (Exception)
import Database.PostgreSQL.Simple as PG
import Debug.Trace (trace)
import Entry
import GUI (Damage (..))

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

push :: Connection -> [Damage] -> IO ()
push c xs = do
  mapM_ f xs
 where
  f :: Damage -> IO ()
  f (Update n _ to) =
    trace q return ()
   where
    q = show n ++ " to " ++ to
