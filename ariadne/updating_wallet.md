How to get latest changes to `wallet-new` from IOHK repo
========================================================

1. Get latest `develop` branch from input-output-hk/cardano-sl repo.
2. Checkout this commit in ariadne repo: `d4057192f4edd60d5f1e872aa452e4a848705e3c`.
3. Give a name to branch, e.g. `git checkout -b update-wallet-new`.
4. Run `import_wallet.sh` script. The script assumes that `cardano-sl` repo is at
`../cardano-sl`, however you can override it by passing path to it as first argument.

 Script must be run from the root of ariadne repo.

5. The script will output a list of modules in wallet-new. Replace the old list in `ariadne.cabal` with it.
6. Commit updates: `git add ariadne/ && git commit -m "Update wallet-new"`.
7. Rebase this branch on master: `git rebase master`.

 If IOHK did not change parts of code that we patched, `git` will do everything automatically.
 Otherwise you will have to resolve all merge confilcts and carefully review their changes.
