{-# LANGUAGE OverloadedStrings #-}

module Database.Interface (connectInfo, connection, pull, push) where

import Control.Exception (SomeException, try)
import Control.Exception.Base (Exception)
import Control.Monad.Trans.Except
import Data.Functor ((<&>))
import Database.PostgreSQL.Simple as PG
import Debug.Trace (trace)
import Entry
import GUI (Damage (..))

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

pull :: Connection -> IO [Entry]
pull conn = PG.query_ conn "select * from clients"

push :: Connection -> [Damage] -> IO Int
push c xs = do
  mapM update xs <&> fromIntegral . sum
 where
  update (Update uuid n from _) =
    case n of
      0 ->
        PG.execute c "update clients set name = ? where id = ?" (from, uuid)
      1 ->
        PG.execute c "update clients set phone = ? where id = ?" (from, uuid)
      _ -> putStrLn "Invalid string" >> return 0
