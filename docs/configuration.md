# Ariadne configuration

Ariadne uses the [Dhall](https://github.com/dhall-lang/dhall-lang)
language for its configuration files and command line options to
override values from files. Example configuration can be
found in the [config/](config) directory.

Path to the configuration file can be specified using `--config` option. By
default `<XdgConfig>/ariadne/ariadne-config.dhall` is used, where
`<XdgConfig>` is a system-dependent standard directory for local
configuration files (`~/.config` on Linux or MacOS, `%APPDATA%` on
Windows). If some value in the configuration is a relative file path,
it's treated as relative to the directory in which main configuration
file (e. g. `ariadne-config.dhall`) is located. There are special
values which can be used in configuration as file paths:
* `@DATA` is a system-dependent standard directory for local
data (`~/.local/share` on Linux or MacOS, `%APPDATA%` on
Windows) suffixed with `/ariadne`.
* `@PWD` is the directory from which Ariadne executable is launched.

If configuration file is not found, default configuration will be used
which is compatible with Cardano Mainnet.

Each option from the configuration can be overridden from the command
line. Use `--help` flag for more details.

Ariadne configuration consists of configurations of several
components: Cardano, Wallet, Update, History. In the following we
describe configuration of each component in detail.

## Cardano configuration

Cardano configuration contains parameters which are used by the
underlying Cardano node.

`configuration-options` defines `Configuration` used by Cardano
node. For more details on that please read the
[documentation](https://github.com/serokell/cardano-sl/blob/fe1e5e07e637e0a6f1396d6edbae9a7a1ef91d31/docs/configuration.md)
the `cardano-sl` repository. If `configuration-path` doesn't exist,
static configuration will be dumped to that path.

`log-config` is the path to the logging configuration (as used by
`log-warper`). If it's not provided a reasonable default configuration
will be used.

`log-prefix` is the path prepended to all paths in logging
configuration. Basically a directory where logs will be written.

`rebuild-db` can be set to `True` to clean node's databases at the
beginning.

`db-path` is the path to the node's databases.

`ekg-params` can be set to enable EKG monitoring (see the `ekg` library).

`default-port` is the default port to bind to.

`node-id` is the identifier of the node in the network.

`topology` is the path to YAML file with the network topology. If it's
not provided default topology will be used to communicate with mainnet
nodes.

## Wallet configuration

Wallet configuration has few values. All of them can be overwritten by
command line arguments.  

`entropy-size` specifies default number of bytes in entropy used to
generate wallets. This value can be overridden by an argument of the
`new-wallet` command.

`keyfile` is the path to the file with secret keys.

`wallet-db-path` is the path to the wallet's database

`stored-db-archives` is the number of stored archives of wallet database. 
This database contains log with all changes of wallet configuration 
like creation or deletion of wallets/account etc. To prevent permanent 
storing of this information, ariadne periodically creates checkpoint
and moves previous logs and checkpoint files to the archive path
and also deletes old archives. This number specifies how many archive files 
should be stored. By default it is 10 files. 

`db-cleanup-period` is the period of archiving previous db's checkpoints in seconds. 
By default it is 600 seconds. 

## Update configuration

Update configuration parameterizes update checker.

`version-check-url` is used to get latest software version.

`update-url` is displayed in the message about new version.

`check-delay` specifies (in seconds) how often we check for a new version.

## History configuration

History configuration has only one value with the key `path`. It's a
path to the file with command history.

## Logging configuration

History configuration has only one value with the key `path`. Logs
produced by Ariadne itself will be in that folder. Note that Cardano
uses different machinery for logging, so its logs are configured by
Cardano configuration.
