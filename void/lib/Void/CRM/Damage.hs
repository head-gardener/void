{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Void.CRM.Damage where

import Codec.Serialise
import Codec.Serialise.Decoding
import Control.Monad (when)
import GHC.Generics
import Void.CRM.Subscriber

data Damage = Update Subscriber | Add Subscriber | Remove Int deriving (Eq, Show, Generic)
instance Serialise Damage where
  decode = do
    len <- decodeMapLen
    when (len /= 1) $ fail $ "invalid map len: " ++ show len
    tag <- decodeString
    case tag of
      "Update" -> Update <$> decode
      "Add" -> Add <$> decode
      "Remove" -> Remove <$> decode
      _ -> fail $ "unexpected tag: " ++ show tag
