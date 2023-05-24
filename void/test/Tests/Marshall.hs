module Tests.Marshall where

import Debug.Trace
import GHC.IO (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck (property)
import qualified Void.CRM.GUI as G
import Void.CRM.Subscriber

test = hspec $ do
  describe "Void.CRM.Subscriber" $ do
    it "can be exported" $ do
      property prop_export

prop_export x =
  unsafePerformIO (G.withNewInstance (`G.push` [x])) `seq` True
