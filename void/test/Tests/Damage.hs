module Tests.Damage where

import Data.List ((\\))
import Debug.Trace
import Test.Hspec
import Test.QuickCheck
import Void.CRM.Damage
import Void.CRM.Subscriber

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
      property prop_distribution

    it "join on the results is a permutation of the original list" $
      property prop_permutation

prop_distribution xs = (count . group) xs == tally xs
 where
  count (us, is, rs) = (length us, length is, length rs)
  tally xs = (do_tally isUpdate, do_tally isInsert, do_tally isRemove)
   where
    do_tally f = length $ filter f xs

prop_permutation xs = permutation xs $ (join . group) xs
 where
  permutation as bs = null (as \\ bs) && null (bs \\ as)
  join (us, is, rs) = map Update us ++ map Insert is ++ map Remove rs
