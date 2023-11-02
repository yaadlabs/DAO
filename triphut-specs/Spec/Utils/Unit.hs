module Spec.Utils.Unit (unitSpec) where

import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
  adaSymbol,
  adaToken,
 )
import PlutusTx.AssocMap as Map
import PlutusTx.Prelude (Bool (False), Integer, ($), (==))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Triphut.Shared (hasSingleToken)

testValue1 :: Value
testValue1 = Value Map.empty

testValue2 :: Value
testValue2 = Value map'
  where
    map' :: Map.Map CurrencySymbol (Map.Map TokenName Integer)
    map' = Map.fromList [(symbol1, (Map.fromList [(tokenName1, 1)]))]

testValue3 :: Value
testValue3 = Value map'
  where
    map' :: Map.Map CurrencySymbol (Map.Map TokenName Integer)
    map' = Map.fromList [(symbol1, (Map.fromList [(tokenName1, 3)]))]

testValue4 :: Value
testValue4 = Value map'
  where
    map' :: Map.Map CurrencySymbol (Map.Map TokenName Integer)
    map' =
      Map.fromList
        [ (symbol1, (Map.fromList [(tokenName1, 1)]))
        , (symbol2, (Map.fromList [(tokenName2, 1)]))
        ]

symbol1 :: CurrencySymbol
symbol1 = CurrencySymbol "sym1"

symbol2 :: CurrencySymbol
symbol2 = CurrencySymbol "sym2"

tokenName1 :: TokenName
tokenName1 = TokenName "tok1"

tokenName2 :: TokenName
tokenName2 = TokenName "tok2"

unitSpec :: [TestTree]
unitSpec =
  [ testCase "hasSingleToken: Empty Value, should fail" $
      assertBool "Should be False" $
        hasSingleToken testValue1 adaSymbol adaToken == False
  , testCase "hasSingletoken: One Symbol, more than 1 of token, should fail" $
      assertBool "Should be False" $
        hasSingleToken testValue3 symbol1 tokenName1 == False
  , testCase "hasSingleToken: Correct CurrencySymbol wrong TokenName, should fail" $
      assertBool "Should Fail at traceError" $
        hasSingleToken testValue4 symbol1 tokenName2 == False
  , testCase "hasSingletoken: One Symbol, should succeed" $
      assertBool "Should be True" $
        hasSingleToken testValue2 symbol1 tokenName1
  , testCase "hasSingletoken: two Symbols, one valid, should succeed" $
      assertBool "Should be True" $
        hasSingleToken testValue4 symbol1 tokenName1
  ]
