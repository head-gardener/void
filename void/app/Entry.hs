{-# LANGUAGE DeriveGeneric #-}

module Entry (Entry (..)) where

import Database.PostgreSQL.Simple as PG
import GHC.Generics

data Entry = Entry
  { uuid :: Int,
    name :: String,
    phone :: String
  }
  deriving (Eq, Show, Generic)

instance PG.FromRow Entry
