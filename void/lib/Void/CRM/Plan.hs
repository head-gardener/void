{-# LANGUAGE DeriveGeneric #-}

module Void.CRM.Plan (Plan (..)) where

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

data Plan = Plan
  { uid :: Int64
  , name :: String
  , rate :: Int
  , minutes :: Int
  }
  deriving (Eq, Show, Generic)

instance Arbitrary Plan where
  arbitrary =
    Plan <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance PG.FromRow Plan
instance Serialise Plan where
  encode (Plan uid name rate minutes) =
    encodeMapLen 4
      <> encode "uid"
      <> encode uid
      <> encode "d_name"
      <> encode name
      <> encode "d_rate"
      <> encode rate
      <> encode "d_minutes"
      <> encode minutes
  decode = do
    len <- decodeMapLen
    when (len /= 4) $ fail $ "invalid map len: " ++ show len
    name <- decodeMapSubscriber "d_name"
    rate <- decodeMapSubscriber "d_rate"
    minutes <- decodeMapSubscriber "d_minutes"
    uid <- decodeMapSubscriber "uid"
    return $ Plan uid name rate minutes
   where
    decodeMapSubscriber tag = do
      tag <- decode
      when (tag /= tag) $ fail $ "unexpected tag " ++ tag
      decode
