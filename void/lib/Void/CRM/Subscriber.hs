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
import Test.QuickCheck

data Subscriber = Subscriber
  { uid :: Int64
  , name :: String
  , phone :: String
  , mou :: Int
  , plan :: Int64
  }
  deriving (Eq, Show, Generic)

instance Arbitrary Subscriber where
  arbitrary =
    Subscriber <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary

instance PG.FromRow Subscriber
instance Serialise Subscriber where
  encode (Subscriber uid name phone mou plan) =
    encodeMapLen 5
      <> encode "uid"
      <> encode uid
      <> encode "d_name"
      <> encode name
      <> encode "d_phone"
      <> encode phone
      <> encode "d_mou"
      <> encode mou
      <> encode "d_plan"
      <> encode plan
  decode = do
    len <- decodeMapLen
    when (len /= 5) $ fail $ "invalid map len: " ++ show len
    name <- decodeMapSubscriber "d_name"
    phone <- decodeMapSubscriber "d_phone"
    mou <- decodeMapSubscriber "d_mou"
    plan <- decodeMapSubscriber "d_plan"
    uid <- decodeMapSubscriber "uid"
    return $ Subscriber uid name phone plan mou
   where
    decodeMapSubscriber tag = do
      tag <- decode
      when (tag /= tag) $ fail $ "unexpected tag " ++ tag
      decode
