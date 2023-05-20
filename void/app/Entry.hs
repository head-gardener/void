{-# LANGUAGE DeriveGeneric #-}

module Entry (Entry (..)) where

import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise (decode, encode), decodeMapSkel)
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Exception (throw)
import Control.Monad (when)
import Data.Int
import Data.Word (Word64)
import Database.PostgreSQL.Simple as PG (FromRow)
import GHC.Generics (Generic)

data Entry = Entry
  { uid :: Int64
  , name :: String
  , phone :: String
  , mou :: Int
  }
  deriving (Eq, Show, Generic)

instance PG.FromRow Entry
instance Serialise Entry where
  encode (Entry uid name phone mou) =
    encodeMapLen 4
      <> encode "uid"
      <> encode uid
      <> encode "d_name"
      <> encode name
      <> encode "d_phone"
      <> encode phone
      <> encode "d_mou"
      <> encode mou
  decode = do
    len <- decodeMapLen
    when (len /= 4) $ fail $ "invalid map len: " ++ show len
    name <- decodeMapEntry "d_name"
    phone <- decodeMapEntry "d_phone"
    mou <- decodeMapEntry "d_mou"
    uid <- decodeMapEntry "uid"
    return $ Entry uid name phone mou
   where
    decodeMapEntry tag = do
      tag <- decode
      when (tag /= tag) $ fail $ "unexpected tag " ++ tag
      decode
