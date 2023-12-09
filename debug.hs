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

---

{ 
 codegen-configs = «derivation /nix/store/kfl0g8ddsbmgpzsz35cjsbq4mjy8647w-codegen-configs.drv»; 

 lambda-buffers-api-docs = «derivation /nix/store/a3pnwcmvgz7adr2b4xwx1f4q3sn8c2iy-lambdabuffers-api-docs.drv»; 

 lambda-buffers-book = «derivation /nix/store/1sg9iz4r16sa0xfpdss6iwrxxnh7bmyn-lambda-buffers-book.drv»; 

 lambda-buffers-codegen-cli = «derivation /nix/store/7l3p9avdp91y57h9czhpw3gsz0k0gghk-lambda-buffers-codegen-exe-lbg-0.1.0.0.drv»; 

 lambda-buffers-codegen-hs-pb = «derivation /nix/store/xd71dsx85pnjnnlrmv9bfcy6d68sh02h-lambda-buffers-codegen-pb.drv»; 

 lambda-buffers-codegen-lib = «derivation /nix/store/g0ci8a5i9c4skk4g7vmli6m5yj480v09-lambda-buffers-codegen-lib-lambda-buffers-codegen-0.1.0.0.drv»; 

 lambda-buffers-codegen-src = «derivation /nix/store/5gq3235zd3z7fxgrpl87r49ixa5m14ng-lambda-buffers-codegen-src.drv»; 

 lambda-buffers-codegen-tests = «derivation /nix/store/k5xjd1xpj4sazi4hjhd4l4jyvi4p3dn9-lambda-buffers-codegen-test-tests-0.1.0.0.drv»; 

 lambda-buffers-compiler-cli = «derivation /nix/store/0v362yn9ifpbn5pw1j4568z5r2qznlk3-lambda-buffers-compiler-exe-lbc-0.1.0.0.drv»; 

 lambda-buffers-compiler-hs-pb = «derivation /nix/store/czkyyh66ya804il7jzmgf6shbg3r7jb8-lambda-buffers-compiler-pb.drv»;

 lambda-buffers-compiler-lib = «derivation /nix/store/s5aalk4320yr5z54n9nl5sh3gfq3qhcd-lambda-buffers-compiler-lib-lambda-buffers-compiler-0.1.0.0.drv»; 

 lambda-buffers-compiler-src = «derivation /nix/store/2s871wpjpv35my0lcsx4ba93lh12dh4g-lambda-buffers-compiler-src.drv»; 

 lambda-buffers-compiler-tests = «derivation /nix/store/rx5053p05aba4d9g77f70ihcpa1y0sam-lambda-buffers-compiler-test-tests-0.1.0.0.drv»; 

 lambda-buffers-frontend-cli = «derivation /nix/store/z2g7ia3yfwi8ipb4i17iqkbb4j572j7h-lambda-buffers-frontend-exe-lbf-0.1.0.0.drv»; 

 lambda-buffers-frontend-lib = «derivation /nix/store/3gw9kq0j9wh6vrnmcd90dxhl5pph2lfc-lambda-buffers-frontend-lib-lambda-buffers-frontend-0.1.0.0.drv»; 

 lambda-buffers-frontend-src = «derivation /nix/store/ca3w30c0s44h2rg2ppg6jdmrc56vlar4-lambda-buffers-frontend-src.drv»; 

 lambda-buffers-frontend-tests = «derivation /nix/store/1ilkcr9mi2cy5wg7jym0sqqib0y7nv1b-lambda-buffers-frontend-test-tests-0.1.0.0.drv»; 

 lambda-buffers-lang-hs-pb = «derivation /nix/store/q8ywxbmqydjwzk481ma5fw9jck00hhj4-lambda-buffers-lang-pb.drv»; 

 lbc = «derivation /nix/store/0v362yn9ifpbn5pw1j4568z5r2qznlk3-lambda-buffers-compiler-exe-lbc-0.1.0.0.drv»; 

 lbf = «derivation /nix/store/i4wckgnaglvyhpls376hj2ih5k1d5wxg-lbf.drv»; 

 lbf-plutarch-example-api = «derivation /nix/store/375d6vcml9qiqfl6cv7gpc64l9ma3akp-lbf-plutarch-example-api-0.1.0.0.drv»; 
 lbf-plutus = «derivation /nix/store/p36pw5jbvyzkfvfjw94b5xbiv5h8wncg-lbf-plutus.drv»; 

 lbf-plutus-golden-api-haskell = «derivation /nix/store/ay87d6y2i24kx7w0zjfykcw7hdzgdv15-lbf-plutus-golden-api-0.1.0.0.drv»; 

 lbf-plutus-golden-api-plutarch = «derivation /nix/store/sz8clrn53kkd1rb0ck0jkcm0v03ak8mb-lbf-plutus-plutarch-golden-api-0.1.0.0.drv»; 

 lbf-plutus-golden-api-purescript = «derivation /nix/store/6mxmi3b0bw2ar2yvk0ag6n73j9krhgcc-lbf-plutus-golden-api-0.1.0.0.drv»; 

 lbf-plutus-haskell = «derivation /nix/store/hrrviw8jyr1s5rcdccf8ngz2wai7rir1-lbf-plutus-0.1.0.0.drv»; 

 lbf-plutus-plutarch = «derivation /nix/store/prx3k23ca3j2ws20aqjfppmhks7ljhq6-lbf-plutus-plutarch-0.1.0.0.drv»; 

 lbf-plutus-purescript = «derivation /nix/store/4x56a94zzqnx6cggl4kgh0yscmiwkvk5-lbf-plutus-0.1.0.0.drv»; 

 lbf-plutus-to-haskell = «derivation /nix/store/0cdg0zd3pjsr3phcbrnmqrqq5y6ffkxm-lbf-plutus-to-haskell.drv»; 

 lbf-plutus-to-plutarch = «derivation /nix/store/5wqy4dlvgcnc0bvyc23falwyifd0rgn6-lbf-plutus-to-plutarch.drv»; 

 lbf-plutus-to-purescript = «derivation /nix/store/cxzc5aqrrmr4l4mbzra145n96r0cx9xd-lbf-plutus-to-purescript.drv»; 

 lbf-prelude = «derivation /nix/store/rs8gh5m2yz5qg19pzygw7kgqjzw11ji0-lbf-prelude.drv»; 

 lbf-prelude-golden-api-haskell = «derivation /nix/store/ddqxn7vfcv7gi9irvb3c0plibnk1hbrc-lbf-prelude-golden-api-0.1.0.0.drv»; 

 lbf-prelude-golden-api-purescript = «derivation /nix/store/chm7x3mxrv1bnh6mkca1kqi8sgcqba5p-lbf-prelude-golden-api-0.1.0.0.drv»; 

 lbf-prelude-haskell = «derivation /nix/store/y6w54bnqjw1xh954xy9wbcp8154jgn4g-lbf-prelude-0.1.0.0.drv»; 

 lbf-prelude-plutarch = «derivation /nix/store/c5phyh557wpvjlycakax4r0qghqafmpn-lbf-prelude-plutarch-0.1.0.0.drv»; 

 lbf-prelude-purescript = «derivation /nix/store/kkyj5yd4h5zk11vlfg9hhhvw2ramng4r-lbf-prelude-0.1.0.0.drv»; 

 lbf-prelude-to-haskell = «derivation /nix/store/cxpijyfjlgmabfv3vrnvfa6pzxqzk7bb-lbf-prelude-to-haskell.drv»; 

 lbf-prelude-to-purescript = «derivation /nix/store/ncqig4f4hwjzx65pc6smpa8w806zj855-lbf-prelude-to-purescript.drv»; 

 lbf-pure = «derivation /nix/store/z2g7ia3yfwi8ipb4i17iqkbb4j572j7h-lambda-buffers-frontend-exe-lbf-0.1.0.0.drv»; 

 lbg = «derivation /nix/store/7l3p9avdp91y57h9czhpw3gsz0k0gghk-lambda-buffers-codegen-exe-lbg-0.1.0.0.drv»; 

 lbg-haskell = «derivation /nix/store/zc7pbz7nrpgbj8w6jf29sa3cmqxv7kxs-lbg-haskell.drv»; 

 lbg-plutarch = «derivation /nix/store/sqvdv9ll0d0aqzy9qqmj3x6yqfy7dgan-lbg-plutarch.drv»; 

 lbg-purescript = «derivation /nix/store/pncdqy80zvw0z3dhxn2sd4daw94z0dgb-lbg-purescript.drv»; 

 lbr-plutarch-lib = «derivation /nix/store/27z7z6hyy2gwv5ljc6nz5m6g1irqql94-lbr-plutarch-lib-lbr-plutarch-0.1.0.0.drv»; 

 lbr-plutarch-src = «derivation /nix/store/3b1w52467bvza629cll1irxv32xmcrrr-lbr-plutus-haskell-src.drv»; 

 lbr-plutarch-tests = «derivation /nix/store/2wzs7ckbnx2lbfgvscrvdjryvk38divx-lbr-plutarch-test-tests-0.1.0.0.drv»; 

 lbr-plutus-haskell-lib = «derivation /nix/store/hxym13g0hs3hqip882lfp6lpxygmg7z0-lbr-plutus-lib-lbr-plutus-0.1.0.0.drv»; 

 lbr-plutus-haskell-src = «derivation /nix/store/bkzrqp741wv0nr6g9sj893xa3gmpxg4j-lbr-plutus-haskell-src.drv»; 

 lbr-plutus-haskell-tests = «derivation /nix/store/76fci9s85mn3lk5fzgmbhn18d0mcn6c0-lbr-plutus-test-tests-0.1.0.0.drv»; 

 lbr-plutus-typescript = «derivation /nix/store/li0zx39m9s1yj0psn3ln2wq45h78jn8f-lbr-plutus-1.0.0.drv»; 

 lbr-plutus-typescript-node2nix = «derivation /nix/store/7i7y0z3m55pnjfz56rpw4dpmji4n4lsc-lbr-plutus-node2nix.drv»; 

 lbr-plutus-typescript-tgz = «derivation /nix/store/45blkcd8hd0bpk6371cprrp6rykz2mrf-lbr-plutus.tgz.drv»; 

 lbr-prelude-derive-rust = «derivation /nix/store/n7j8zmq56yrww00rh9zgjr89iqbr7w9k-lbr-prelude-derive-0.1.0.drv»; 

 lbr-prelude-haskell-src = «derivation /nix/store/l92058xgl1lrxw7iihjarv1cx60g4jdi-lbr-prelude-haskell-src.drv»; 

 lbr-prelude-rust = «derivation /nix/store/gxlpgbb9y64aknrcr64d179s9agyh23b-lbr-prelude-0.1.0.drv»; 

 lbr-prelude-typescript = «derivation /nix/store/97b6kbb2pi0yw89h8yirvy0rhnavl5hl-lbr-prelude-1.0.0.drv»; 

 lbr-prelude-typescript-node2nix = «derivation /nix/store/368vcc7z9k7gla0pm5hsbw2kbvfyxwws-lbr-prelude-node2nix.drv»; 

 lbr-prelude-typescript-tgz = «derivation /nix/store/6r6gi4cwjm6vpfw91j7amkafayy452q3-lbr-prelude.tgz.drv»; 

 "lbr-prelude:lib:lbr-prelude" = «derivation /nix/store/n5arxa55jb38hid13hnwa7crxra95gg4-lbr-prelude-lib-lbr-prelude-0.1.0.0.drv»; 

 "lbr-prelude:test:tests" = «derivation /nix/store/4afra25lcdl5z0wnrp7kmwg19nzb7cgr-lbr-prelude-test-tests-0.1.0.0.drv»; 

 lbt-plutus-golden-haskell = «derivation /nix/store/l6bnjnnw0glrxwlhwsj5yha78jk8i5dk-lbt-plutus-golden-data.drv»; 

 lbt-plutus-golden-purescript = «derivation /nix/store/da47rw7rqr5n4c9mk3l3bmjm9nijx78z-lbt-plutus-golden-data.drv»;

 lbt-plutus-haskell-golden-cli = error: builder for '/nix/store/7m3gbf6brlv3i
