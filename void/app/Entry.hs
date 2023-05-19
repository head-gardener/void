{-# LANGUAGE DeriveGeneric #-}

module Entry (Entry (..)) where

import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise (encode))
import Codec.Serialise.Encoding
import Database.PostgreSQL.Simple as PG (FromRow)
import GHC.Generics (Generic)

data Entry = Entry
  { uuid :: Int
  , name :: String
  , phone :: String
  , mou :: Int
  }
  deriving (Eq, Show, Generic)

instance PG.FromRow Entry
instance Serialise Entry where
  encode (Entry uuid name phone mou) =
    encodeMapLen 4
      <> encode "uuid"
      <> encode uuid
      <> encode "d_name"
      <> encode name
      <> encode "d_phone"
      <> encode phone
      <> encode "d_mou"
      <> encode mou
