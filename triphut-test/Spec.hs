{- | Module for running all the tests specified in the 'Spec' modules -}
module Main (main) where

import Prelude (IO, ($))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.Utils qualified as Utils
import Spec.ConfigurationNft qualified as ConfigurationNft
import Spec.Index qualified as Index
import Spec.Tally qualified as Tally
import Spec.Vote qualified as Vote
import Spec.Upgrade qualified as Upgrade
import Spec.VoteValidator qualified as VoteValidator
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "triphut"
      [ -- Utils.spec
      -- , ConfigurationNft.spec
      -- , Index.spec
      -- , Tally.spec
      -- , Vote.spec
        --Upgrade.spec
        VoteValidator.spec
      ]
