{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- module GUI (push, GUI.drop, withNewWindow, wait, VoidInstance) where
module GUI where

import Codec.Serialise (Serialise, deserialise)
import Codec.Serialise.Class (decode)
import Codec.Serialise.Decoding
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Debug.Trace (trace)
import Entry
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics

foreign import ccall "void_gui_init"
  void_gui_init :: VoidInstance

foreign import ccall "void_gui_exec"
  void_gui_exec :: VoidInstance -> CInt

foreign import ccall "void_gui_finish"
  void_gui_finish :: VoidInstance -> CInt

foreign import ccall "void_gui_add"
  void_gui_add :: CUInt -> Ptr CChar -> Ptr CChar -> VoidInstance -> CInt

foreign import ccall "void_gui_drop"
  void_gui_drop :: VoidInstance -> CInt

foreign import ccall "void_gui_pull_damage"
  void_gui_pull_damage :: VoidInstance -> Ptr CInt

foreign import ccall "void_gui_free_damage"
  void_gui_free_damage :: Ptr CInt -> CInt

-- compiler might decide to free this object at any point which would be a
-- real nuisance
newtype VoidInstance = VoidInstance (Ptr VoidInstance) deriving (Eq)

data Damage = Update Int Int String String deriving (Eq, Show, Generic)
instance Serialise Damage where
  decode = do
    len <- decodeMapLen
    when (len /= 1) $ fail $ "invalid map len: " ++ show len
    tag <- decodeString
    len <- decodeListLen
    case (tag, len) of
      ("Update", 4) -> Update <$> decode <*> decode <*> decode <*> decode
      _ -> fail $ "unexpected (tag, len): " ++ show (tag, len)

nullWindow :: VoidInstance
nullWindow = VoidInstance nullPtr

withNewInstance :: (VoidInstance -> IO ()) -> IO ()
withNewInstance f =
  case newWindow of
    Just w -> do
      f w
      void_gui_finish w `seq` return ()
    Nothing -> putStrLn "Initialization failure"

newWindow :: Maybe VoidInstance
newWindow = void_gui_init `seq` checkWindow void_gui_init
 where
  checkWindow :: VoidInstance -> Maybe VoidInstance
  checkWindow x
    | x == nullWindow = Nothing
    | otherwise = Just x

wait :: VoidInstance -> CInt
wait = void_gui_exec

-- not super efficient cause of marshalling and all.
push :: VoidInstance -> [Entry] -> IO ()
push window =
  mapM_ $
    \x -> withCString (name x) $ \cn ->
      withCString (phone x) $ \ct -> do
        void_gui_add (fromIntegral $ uuid x) cn ct window `seq` return ()

drop :: VoidInstance -> CInt
drop = void_gui_drop

pull :: VoidInstance -> IO [Damage]
pull w = do
  cs <- withDamage behead
  bs <- BL.fromStrict <$> B.packCStringLen cs
  return $ Codec.Serialise.deserialise bs
 where
  behead :: Ptr CInt -> IO CStringLen
  behead ptr = do
    len <- peek ptr
    xs <- peek $ castPtr $ plusPtr ptr 8 -- why?
    return (xs, fromIntegral len)

  withDamage :: (Ptr CInt -> IO a) -> IO a
  withDamage f =
    let ptr = void_gui_pull_damage w
     in f ptr >>= \x -> x `seq` void_gui_free_damage ptr `seq` return x
