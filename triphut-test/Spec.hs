module Main (main) where

import Prelude (IO, ($))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.Utils qualified as Utils
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "triphut"
      [Utils.spec]
