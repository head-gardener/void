{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- module GUI (push, GUI.drop, withNewWindow, wait, VoidWindow) where
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
  void_gui_init :: VoidWindow

foreign import ccall "void_gui_exec"
  void_gui_exec :: VoidWindow -> CInt

foreign import ccall "void_gui_finish"
  void_gui_finish :: VoidWindow -> CInt

foreign import ccall "void_gui_add"
  void_gui_add :: Ptr CChar -> Ptr CChar -> VoidWindow -> CInt

foreign import ccall "void_gui_drop"
  void_gui_drop :: VoidWindow -> CInt

foreign import ccall "void_gui_pull_damage"
  void_gui_pull_damage :: VoidWindow -> Ptr CInt

foreign import ccall "void_gui_free_damage"
  void_gui_free_damage :: Ptr CInt -> CInt

-- compiler might decide to free this object at any point which would be a
-- real nuisance
newtype VoidWindow = VoidWindow (Ptr VoidWindow) deriving (Eq)

data Damage = Update Int String String deriving (Eq, Show, Generic)
instance Serialise Damage where
  decode = do
    len <- decodeMapLen
    when (len /= 1) $ fail $ "invalid map len: " ++ show len
    tag <- decodeString
    len <- decodeListLen
    case (tag, len) of
      ("Update", 3) -> Update <$> decode <*> decode <*> decode
      _ -> fail $ "unexpected (tag, len): " ++ show (tag, len)

nullWindow :: VoidWindow
nullWindow = VoidWindow nullPtr

withNewWindow :: (VoidWindow -> IO ()) -> IO ()
withNewWindow f =
  case newWindow of
    Just w -> do
      f w
      void_gui_finish w `seq` return ()
    Nothing -> putStrLn "Initialization failure"

newWindow :: Maybe VoidWindow
newWindow = void_gui_init `seq` checkWindow void_gui_init
 where
  checkWindow :: VoidWindow -> Maybe VoidWindow
  checkWindow x
    | x == nullWindow = Nothing
    | otherwise = Just x

wait :: VoidWindow -> CInt
wait = void_gui_exec

-- not super efficient cause of marshalling and all.
push :: VoidWindow -> [Entry] -> IO ()
push window (e : es) =
  withCString (name e) $ \cn ->
    withCString (phone e) $ \ct -> do
      let code = void_gui_add cn ct window
      code `seq` push window es
push window [] =
  return ()

drop :: VoidWindow -> CInt
drop = void_gui_drop

pull :: VoidWindow -> IO [Damage]
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
     in f ptr >>= \x ->
          x `seq` void_gui_free_damage ptr `seq` return x
