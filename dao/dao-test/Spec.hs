-- | Module for running all the tests specified in the 'Spec' modules
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.ConfigurationNft qualified as ConfigurationNft
import Spec.Index qualified as Index
import Spec.Tally qualified as Tally
import Spec.Treasury qualified as Treasury
import Spec.Upgrade qualified as Upgrade
import Spec.Utils qualified as Utils
import Spec.Vote qualified as Vote
import Spec.VoteValidator qualified as VoteValidator
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO, ($))

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "Triphut"
      [ -- Utils.spec
        ConfigurationNft.spec
      -- , Index.spec
      -- , Tally.spec
        -- , Vote.spec
        -- , VoteValidator.spec
        -- , Treasury.spec
        -- , Upgrade.spec
      ]
