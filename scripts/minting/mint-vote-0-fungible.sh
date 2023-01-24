./scripts/minting/test-mint-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-0.skey \
  scripts/test-policies/test-policy-1.plutus \
  $(cat scripts/test-policies/test-policy-1-id.txt) \
  544F4B454E \
  2
