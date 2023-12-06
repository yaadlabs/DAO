-- Both the same it seems

-- With the old implementation

Just
  [
    ( TxOutRef
        { txOutRefId = 01 f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53
        , txOutRefIdx = 0
        }
    , TxOut
        { txOutAddress =
            Address
              { addressCredential = PubKeyCredential 73 dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0
              , addressStakingCredential = Nothing
              }
        , txOutValue =
            Value
              { getValue =
                  Map
                    { unMap =
                        [ (,Map {unMap = [("", 10000000)]})
                        , (00000000000000000000000000000000000000000000000000000000, Map {unMap = [("config", 1)]})
                        ]
                    }
              }
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }
    )
  ]

-- With new implementation

Just
  [
    ( TxOutRef {txOutRefId = 01 f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53, txOutRefIdx = 0}
    , TxOut
        { txOutAddress =
            Address
              { addressCredential = PubKeyCredential 73 dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0
              , addressStakingCredential = Nothing
              }
        , txOutValue =
            Value
              { getValue =
                  Map
                    { unMap =
                        [ (,Map {unMap = [("", 10000000)]})
                        , (00000000000000000000000000000000000000000000000000000000, Map {unMap = [("config", 1)]})
                        ]
                    }
              }
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }
    )
  ]
