import Test.Hspec
import qualified Void.CRM.GUI as G
import Void.CRM.Subscriber

main :: IO ()
main = hspec $ do
  describe "Void.CRM.GUI" $ do
    it "can export subscribers" $ do
      G.withNewInstance (\w -> G.push w [Subscriber 0 "Vova" "123" 0])
