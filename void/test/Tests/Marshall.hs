module Tests.Marshall where

import Test.Hspec
import qualified Void.CRM.GUI as G
import Void.CRM.Subscriber

test = hspec $ do
  describe "Void.CRM.GUI" $ do
    it "can export subscribers" $ do
      G.withNewInstance (\w -> G.push w [Subscriber 0 "Vova" "123" 0])
