# ![](./img/logo.png) Ariadne

Ariadne is a cryptocurrency wallet developed by
[Serokell](https://serokell.io/). It has two user interfaces: terminal
user interface (TUI) and graphical user interface (GUI). At the time
being it supports only the [Cardano
cryptocurrency](https://www.cardano.org/), but there are plans to
support multiple currencies in the future.

## Features [↑](#-ariadne)

* [x] Wallet management
* [x] BIP-39 mnemonics
* [x] BIP-44 compatibility
* [ ] Compatibility with Daedalus
* [x] Balances of wallets, accounts, address
* [ ] Transaction history
* [x] Transaction sending
* [x] Spending passwords
* [x] Optimized block storage (compared to Daedalus)
* [x] [Knit](knit/README.md) language
* [x] Notifications about new software versions
* [ ] Automatic software updates
* [ ] Integration with hardware wallets
* [ ] Staking and delegation

### Wallet management

Create or delete a hierarchical deterministic (HD) wallet (BIP-32) or
an account inside a wallet. Generate a new address inside an
account. Wallets and accounts have names.

### BIP-39 mnemonics

Ariadne uses the
[BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
specification to work with mnemonics. When you generate a new wallet,
you will see a mnemonic which you should save into a reliable
place. Later you can use this mnemonic to restore your wallet. Number
of words in mnemonic is configurable.

### BIP-44 compatibility

Addresses are generated according to the
[BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki)
specification. This derivation scheme is one of the most commonly used
schemes in cryptocurrency wallets.

### Compatibility with Daedalus

Unfortunately Ariadne is not compatible with Daedalus due to
differences in HD derivation schemes.  It means that you can't use
your existing wallet generated by Daedalus. However, you can send a
transaction from your existing wallet to your new Ariadne wallet. We
plan to provide limited support for derivation schemes different from
BIP-44 so that you'll be able to use a wallet generated by Daedalus in
Ariadne.

### Get the balance of a wallet/account/address

It's possible to see the balance of any of your wallets, accounts and addresses.

### Transaction history

Transaction history is not supported yet.

### Transaction sending

Transactions can be sent from a whole wallet or a set of accounts
belonging to a wallet. More than one output can be specified.

### Spending passwords

Each wallet can be protected with its own spending password. It's
impossible to send a transaction from a wallet if one doesn't know its
spending password.

### Optimized block storage

Daedalus stores blocks in plain files. Each block is stored in its own
file. Since number of blocks is large, it leads to excessive
fragmentation. Ariadne stores blocks in a RocksDB database. It reduces
number of files by order of magnitude and the size of block database
is about 8 times smaller.

### Knit

All actions correspond to commands in the [Knit](knit/README.md)
language and can be typed in a console. Advanced users can execute
Knit commands instead of using UI. The console supports completion.

### Notifications about new software versions

If a new version of Ariadne is released, you will see a notification
in UI.

### Automatic software updates

Later it will be possible to automatically download and install
software updates when a new version is released.

### Integration with hardware wallets

We plan to support hardware wallets in the future.

### Staking and delegation

Staking and delegation will be supported in Ariadne after they are
specified and implemented in the core protocol.


## Building from Source Code [↑](#-ariadne)

### Supported platforms

Ariadne TUI can be built and works on Linux and macOS.

Qt GUI only works on Linux. Windows and macOS will be supported soon.

### Build using Nix

Set up Serokell binary cache so that you don't have to build dependencies:

```sh
sudo $(nix-build pkgs.nix -A cachix --no-out-link)/bin/cachix use serokell
```

If you are on a single-user Nix install (`nix-shell -p nix-info --run nix-info`
should say `multi-user?: no`), omit `sudo` in the command above.

If you are on NixOS, make sure to add `https://cache.nixos.org` to `nix.binaryCaches`,
otherwise main Nix binary cache stops working. See [cachix/cachix#128][].

[cachix/cachix#128]: https://github.com/cachix/cachix/pull/128

Then, install Nix with [NixOS/nix#2409][] patch:

```sh
nix-env -f pkgs.nix -iA nix
```

[NixOS/nix#2409]: https://github.com/NixOS/nix/pull/2409

For production builds, run `nix-build`.

For incremental builds, run `nix-shell`. Then, use either `stack build` or
`cabal new-build all` as you normally would. This will only build local packages,
all dependencies are managed by Nix.

### Build using Stack

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).
2. Make sure you have the following system libraries installed:
  - RocksDB
  - OpenSSL
  - Qt 5 (if you want to build GUI)
  - ICU
3. Run `stack build ariadne-vty-app` to build terminal text-based interface
and `stack build ariadne-qt-app` for Qt-based GUI. Or just `stack build` for both.

#### macOS


If you have Homebrew, run:

```sh
brew install bash coreutils icu4c rocksdb openssl qt xz
```

If you haven't had `bash` previously installed, close terminal session
and then open a new one. You will also need to add icu and OpenSSL to
`~/.stack/config.yaml`:

```yaml
extra-include-dirs:
- /usr/local/opt/openssl/include
- /usr/local/opt/icu4c/include
extra-lib-dirs:
- /usr/local/opt/openssl/lib
- /usr/local/opt/icu4c/lib
```

Now, to build, run:

```sh
QTAH_QMAKE=/usr/local/opt/qt/bin/qmake stack build ariadne
```

## Configuration [↑](#-ariadne)

Ariadne is configurable via configuration files and command line
options. Default configuration is provided out of the box and is
compatible with Cardano SL mainnet. That should be sufficient in many
cases. Detailed description of Ariadne configuration can be found in
[the corresponding document](docs/configuration.md).

## Usage [↑](#-ariadne)

Launch `ariadne` executable to use TUI. If you [used
Stack](#build-using-stack) to build Ariadne, type `stack exec --
ariadne` to launch the executable. In order to use GUI launch
`ariadne-qt`.

For complete usage guide visit [TUI](docs/usage-tui.md) or [GUI](docs/usage-gui.md) page.

## Issue tracker

We use [YouTrack](https://issues.serokell.io/issues/AD) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors [↑](#-ariadne)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
