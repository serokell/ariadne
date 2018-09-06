How to get latest updates to `wallet-new` from IOHK repo
========================================================

1. Get latest `develop` branch from `input-output-hk/cardano-sl` repo.
2. Copy the `import_wallet.sh` script to the root of the Ariadne repo.
3. Checkout `d4057192f4edd60d5f1e872aa452e4a848705e3c` in the Ariadne repo, e.g.
   `git checkout d4057192f4edd60d5f1e872aa452e4a848705e3c -b update-wallet-new`.
4. Run `import_wallet.sh`. The script assumes that `cardano-sl` repo is at
`../cardano-sl`, however you can override it by passing path to it as first argument. The script must be run from the root of the Ariadne repo.
5. The script will output a list of modules in wallet-new. Replace the old list in `ariadne.cabal` with it.
6. Commit updates: `git add ariadne/ && git commit -m "Update wallet-new"`.
7. Rebase this branch on master: `git rebase master`.

If IOHK did not change parts of code that we patched, `git` will do everything automatically.
Otherwise you will have to resolve all merge confilcts and carefully review their changes.

Note: Ariadne was last synced with the Cardano repostitory at Cardano commit [`0fa0ce646cb83af8028878a1eed169f513a987b6`](https://github.com/input-output-hk/cardano-sl/tree/0fa0ce646cb83af8028878a1eed169f513a987b6) (2018-08-01).
