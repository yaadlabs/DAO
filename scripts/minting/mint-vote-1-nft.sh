./scripts/minting/test-mint-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-1.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-1.skey \
  scripts/test-policies/test-policy-0.plutus \
  $(cat scripts/test-policies/test-policy-0-id.txt) \
  31 \
  1
