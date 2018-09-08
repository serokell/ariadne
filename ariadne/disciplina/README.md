# Disciplina Wallet

Ariadne-based wallet for Disciplina.

Two frontends are provided:
* `dscp-wallet` — Vty-based UI
* `dscp-wallet-cli` — Basic Knit command line

Running the wallet requires specifying a witness node address:
```
dscp-wallet --witness witness-1.disciplina.serokell.team
```

Some useful Knit commands:
```
knit> list-keys
L (L "Some wallet"
     "UKZbPfVbHQrsNTfBdtKKwRtQDzWOGmY3eh9sUWlsvNSAp1ggDgRRmJDWEzHh/43JfQ1LcmsklGS43ZLVFT
     "3ePzBz82cJCSkCUWc/DmxxH8Ahj76+o4G3osAHDKmM0="
     LL4qKkivDC1qzvT9ovcfr2uqx5Esi7e5ZbKCrvKYC1jEi6a3kuuAjrqE)
  (L "Another wallet"
     "UEqIlL9CD3wAdeoCfI6XyrtQ80AbwrmZbxD7+1CyikWLmVggbgRr/vFeJX0nSsH1GGjkLfORSfrfL5B1q9
     "h6S36CJw+fevz8JiHyok9kW3Rwy+8+Ijj9go8xtHdGY="
     LL4qKo9KtKiWEaaoDVnZhKW1BiQwyPjbDxd77aMgUDa43sqFjxb2Mub7)

knit> gen-key-pair
L ""                                                                                              -- wallet name
  "v1xsibY+BBfKPOvAI3um0+TGRLGs45E4PFOfZYPuHmI="                                                  -- unencrypted secret key
  "D/nSS++ZrpsxqjV/Pj38iH75FJP73YBqyTyKUbqqfck="                                                  -- public key
  LL4qKnmSQJJB3hdRKdL1J61HJYFzSwChWjcoZqzHPFdSdzyLZmdSSKiD                                        -- address

knit> gen-key-pair "passphrase" "And another one"
L "And another one"
  "UBLf8FurpezJgVIFx6SHJMNQtz9eW8GTQs8y7eR5SzKDYVggFbb449bpHZIimt/H+bhjMyIRLj4aTKdpjFsFJPGS4Rg="  -- encrypted secret key
  "v6NE7nYvDzJDYGZ5hZnW4oBDmsmWL+l1L2jpFy3s3+M="                                                  -- public key
  LL4qKtozvcr44DA3rXDDy7kX94aseq5XfnZ2yYbmUNDG316pdEfTo7WD                                        -- address

knit> send-tx "UDpQX35yW72QVKcL4ufC7YtQMDcPghbX+nKcccrRpHNLZFggwgj8Zo0cbRTepMzHudhyuatPGP5isWJNuNu7vS1ONBs=" (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10)
"6745d05dcd39a97faa735be93e64a669fc73ac569f301236e87ec85814ff227b"  -- transaction id

knit> send-tx "UBLf8FurpezJgVIFx6SHJMNQtz9eW8GTQs8y7eR5SzKDYVggFbb449bpHZIimt/H+bhjMyIRLj4aTKdpjFsFJPGS4Rg=" "passphrase" (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10)
"a4355afd95cfde8ea84c42155dfe7018bcd7d41ce5a4fc1bcd3ddb2879f489ce"  -- transaction id

knit> get-balance LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp
1337
```

More info about Vty UI is available in [Ariadne TUI user guide](https://github.com/serokell/ariadne/blob/master/docs/usage-tui.md)