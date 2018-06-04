# ![](./img/logo.png) Ariadne

Ariadne is a cryptocurrency wallet developed by
[Serokell](https://serokell.io/). It has two user interfaces: terminal
user interface (TUI) and graphical user interface (GUI). At the time
being it supports only the [Cardano
cryptocurrency](https://www.cardano.org/), but there are plans to
support multiple currencies in the future.

## Features [↑](#-ariadne)

* [x] Create a new HD wallet (BIP-32), account or address
* [x] Delete or rename wallets/accounts
* [x] Import secret keys used by Daedalus
* [x] BIP-39 mnemonics
* [ ] BIP-44 compatibility
* [x] Get the balance of a wallet/account/address
* [ ] Get transaction history
* [x] Send a transaction from a wallet
* [x] Spending passwords
* [x] Optimized block storage (compared to Daedalus)
* [x] All actions correspond to commands in the [Knit](knit/README.md)
language and can be typed in a console
* [ ] Notifications about new software versions
* [ ] Automatic software updates
* [ ] Integration with hardware wallets
* [ ] Staking and delegation

## Building from Source Code [↑](#-ariadne)

### Supported platforms

Ariadne TUI can be built and works on Linux and Mac OS.

GUI works only on Linux. Windows and Mac OS will be supported soon.

### Build using Stack

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).
2. Make sure you have the following system libraries installed:
  - RocksDB
  - OpenSSL
  - Qt 5 (if you want to build GUI)
  - ICU
3. Run `stack build ariadne`.

#### Mac OS

If you are using Mac OS, it may be necessary to add the following
lines to `~/.stack/config.yaml`:

```
extra-include-dirs:
- /usr/local/opt/openssl/include
- /usr/local/opt/icu4c/include
extra-lib-dirs:
- /usr/local/opt/openssl/lib
- /usr/local/opt/icu4c/lib
```

## Configuration [↑](#-ariadne)

Ariadne uses the [Dhall](https://github.com/dhall-lang/dhall-lang)
language for its configuration files. Example configuration can be
found in the [config/](config) directory. This configuration is
compatible with Cardano SL mainnet.

Path to the configuration file can be specified using `--config` option. By
default `<XdgConfig>/ariadne/ariadne-config.dhall` is used, where
`<XdgConfig>` is a system-dependent standard directory for local
configuration files (`~/.config` on Linux or MacOS, %APPDATA% on
Windows). If some value in the configuration is a relative file path,
it's treated as relative to the directory in which main configuration
file (e. g. `ariadne-config.dhall`) is located. There are special
values which can be used in configuration as file paths:
* `@DATA` is a system-dependent standard directory for local
data (`~/.local/share` on Linux or MacOS, %APPDATA% on
Windows) suffixed with `/ariadne`.
* `@PWD` is the directory from which Ariadne executable is launched.

Each option from the configuration can be overridden from the command
like. Use `--help` flag for more details.

## Usage [↑](#-ariadne)

Launch `ariadne` executable to use TUI. If you [used
Stack](#build-using-stack) to build Ariadne, type `stack exec --
ariadne` to launch the executable. In order to use GUI launch
`ariadne-qt`. Make sure to copy [configuration](#Configuration) into
the default location or pass its location explicitly.

### TUI

By default the `Wallet` tab is open which has a bunch of widgets. To
switch between widgets use `Tab` and `Shift-Tab`. To change active
tab, press `Esc` and use arrows or press a hotkey (they will be
highlighted). The `Help` tab displays all available commands, their
short description and information about arguments. The `Logs` tab
displays logs produced by Cardano node and Ariadne itself.

Mouse actions are also supported, you can press buttons or even scroll
widgets using mouse. Hold `Shift` to select text using mouse.

## For Contributors [↑](#-ariadne)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
