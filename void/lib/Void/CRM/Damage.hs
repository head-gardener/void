{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Void.CRM.Damage where

import Codec.Serialise
import Codec.Serialise.Decoding
import Control.Monad (when)
import Data.Int
import GHC.Generics
import Void.CRM.Subscriber

data Damage = Update Subscriber | Insert Subscriber | Remove Int64 deriving (Eq, Show, Generic)
instance Serialise Damage where
  decode = do
    len <- decodeMapLen
    when (len /= 1) $ fail $ "invalid map len: " ++ show len
    tag <- decodeString
    case tag of
      "Update" -> Update <$> decode
      "Insert" -> Insert <$> decode
      "Remove" -> Remove <$> decode
      _ -> fail $ "unexpected tag: " ++ show tag

isUpdate (Update _) = True
isUpdate _ = False

isInsert (Insert _) = True
isInsert _ = False

isRemove (Remove _) = True
isRemove _ = False

group = do_group ([], [], [])
 where
  do_group r [] = r
  do_group (u, i, r) (Update x : xs) = do_group (x : u, i, r) xs
  do_group (u, i, r) (Insert x : xs) = do_group (u, x : i, r) xs
  do_group (u, i, r) (Remove x : xs) = do_group (u, i, x : r) xs
