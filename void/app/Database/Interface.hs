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
  mapM f xs >>= (\x -> putStrLn ("changed " ++ show x ++ " rows")) . sum
 where
  f (Update uuid n from _) =
    case n of
      0 ->
        PG.execute c "update clients set name = ? where id = ?" (from, uuid)
      1 ->
        PG.execute c "update clients set phone = ? where id = ?" (from, uuid)
      _ -> putStrLn "Invalid string" >> return 0
