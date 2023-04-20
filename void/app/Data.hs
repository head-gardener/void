{-# LANGUAGE DeriveGeneric #-}

module Data (Entry (..)) where

import Database.PostgreSQL.Simple as PG
import GHC.Generics

data Entry = Entry
  { name :: String,
    phone :: String
  }
  deriving (Eq, Show, Generic)

instance PG.FromRow Entry
