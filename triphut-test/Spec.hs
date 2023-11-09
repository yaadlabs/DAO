{- | Module for running all the tests specified in the 'Spec' modules -}
module Main (main) where

import Prelude (IO, ($))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.Utils qualified as Utils
import Spec.ConfigurationNft qualified as ConfigurationNft
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "triphut"
      [ Utils.spec
      , ConfigurationNft.spec
      ]
