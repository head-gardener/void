{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- module GUI (push, GUI.drop, withNewWindow, wait, VoidInstance) where
module Void.CRM.GUI where

import Codec.Serialise (Serialise, deserialise, serialise)
import Codec.Serialise.Class (decode)
import Codec.Serialise.Decoding
import Control.Monad (liftM, when)
import Data.ByteString
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Database.PostgreSQL.Simple (Connection)
import Debug.Trace (trace, traceShow)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Base (ap)
import GHC.Generics
import Void.CRM.Damage
import Void.CRM.Subscriber

foreign import ccall "void_gui_init"
  void_gui_init :: VoidInstance

foreign import ccall "void_gui_exec"
  void_gui_exec :: VoidInstance -> CInt

foreign import ccall "void_gui_finish"
  void_gui_finish :: VoidInstance -> CInt

foreign import ccall "void_gui_add"
  void_gui_add :: VoidInstance -> CInt -> CInt -> Ptr CChar -> ()

foreign import ccall "void_gui_drop"
  void_gui_drop :: VoidInstance -> ()

foreign import ccall "void_gui_drain_damage"
  void_gui_drain_damage :: VoidInstance -> Ptr CInt

foreign import ccall "void_gui_free_damage"
  void_gui_free_damage :: Ptr CInt -> CInt

foreign import ccall "void_gui_status"
  void_gui_status :: Ptr CChar -> VoidInstance -> ()

foreign import ccall "void_gui_parse"
  void_gui_parse :: CInt -> Ptr CChar -> CInt

-- compiler might decide to free this object at any point which would be a
-- real nuisance
newtype VoidInstance = VoidInstance (Ptr VoidInstance) deriving (Eq)

nullWindow :: VoidInstance
nullWindow = VoidInstance nullPtr

withNewInstance :: (VoidInstance -> IO ()) -> IO ()
withNewInstance f =
  case newWindow of
    Just w -> do
      f w
      void_gui_finish w `seq` return ()
    Nothing -> Prelude.putStrLn "Initialization failure"

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
push :: Serialise a => VoidInstance -> CInt -> [a] -> IO ()
push window n =
  mapM_ $
    \x ->
      useAsCStringLen
        ((toStrict . serialise) x)
        (\(s, len) -> void_gui_add window n (fromIntegral len) s `seq` return ())

drop :: VoidInstance -> ()
drop = void_gui_drop

putStatus :: VoidInstance -> String -> IO ()
putStatus w s =
  withCString s $ \s ->
    void_gui_status s w `seq` return ()

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
    let ptr = void_gui_drain_damage w
     in f ptr >>= \x -> x `seq` void_gui_free_damage ptr `seq` return x

tryParse :: Subscriber -> IO CInt
tryParse x =
  useAsCStringLen
    ((toStrict . serialise) x)
    (\(s, len) -> return (void_gui_parse (fromIntegral len) s))
