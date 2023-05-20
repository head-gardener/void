module Tests.Damage where

import Debug.Trace
import Test.Hspec
import Test.QuickCheck
import Void.CRM.Damage
import Void.CRM.Subscriber

instance Arbitrary Subscriber where
  arbitrary = Subscriber <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Damage where
  arbitrary =
    frequency
      [ (2, Update <$> arbitrary)
      , (1, Insert <$> arbitrary)
      , (1, Remove <$> arbitrary)
      ]

test = hspec $ do
  describe "Void.CRM.Damage.group" $ do
    it "correctly distributes arbitrary data" $
      property distribution

distribution ds = (count . group) ds == tally ds
 where
  count (us, is, rs) = (length us, length is, length rs)
  tally ds = (do_tally isUpdate, do_tally isInsert, do_tally isRemove)
   where
    do_tally f = length $ filter f ds
