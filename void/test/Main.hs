module Main where

import Tests.Damage
import Tests.Marshall

main = do
  Tests.Marshall.test
  Tests.Damage.test
