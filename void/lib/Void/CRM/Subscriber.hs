{-# LANGUAGE DeriveGeneric #-}

module Void.CRM.Subscriber (Subscriber (..)) where

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

data Subscriber = Subscriber
  { uid :: Int64
  , name :: String
  , phone :: String
  , mou :: Int
  }
  deriving (Eq, Show, Generic)

instance PG.FromRow Subscriber
instance Serialise Subscriber where
  encode (Subscriber uid name phone mou) =
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
    name <- decodeMapSubscriber "d_name"
    phone <- decodeMapSubscriber "d_phone"
    mou <- decodeMapSubscriber "d_mou"
    uid <- decodeMapSubscriber "uid"
    return $ Subscriber uid name phone mou
   where
    decodeMapSubscriber tag = do
      tag <- decode
      when (tag /= tag) $ fail $ "unexpected tag " ++ tag
      decode
