# Ariadne TUI user guide

This is a user guide for Ariadne text-based user interface (TUI), which runs 
in any terminal emulator.

## About mouse support

Almost any action accomplishable with a keyboard can also be performed with 
a mouse in an intuitive way. This guide will mostly focus on keyboard 
shortcuts, only mentioning mouse where its usage is not obvious.

Furthermore, please note that while Ariadne has no native support for text 
selection with a mouse (yet), you can still select text by using your terminal 
emulator's features. In Linux, you usually need to hold `Shift` while 
selecting. In macOS, you should try holding `Alt` or `Fn`, or consult your 
terminal emulator's documentation.

## Screen layout and navigation

Ariadne window consists of several tabs, presented at the top of the screen. 
Each tab has a number of widgets.

![](../img/main_screen.png)

There are two ways of navigating the tabs and widgets: with shortcuts and by 
using the `Tab` key. To access shortcuts, press the `Esc` key. There will 
appear a red underlined letter in front of each tab and widget. To switch 
to the desired tab or widget, press the corresponding letter. For example, 
to switch to the logs tab, press `Esc` and then `l`. The currently active 
widget will show a red dot on its first line and have brighter text.

The `Tab` key provides you with the usual cyclic navigation: pressing `Tab` 
moves selection one widget forward, and pressing `Shift-Tab` &mdash; one widget 
backward.

Here are all the tabs present in Ariadne:
- `Wallet`: main tab, where you can do all the wallet operations
- `Help`: a short help about `Knit` commands
- `About`: an about screen with licensing information
- `Logs`: logs from an embedded Cardano node, which may be useful should 
  something go wrong

All widgets that have a lot of text (e.g. About and Logs) can be scrolled with 
mouse, arrow keys or `j`/`k` keys.

## Wallet tab &mdash; the heart of Ariadne

In the `Wallet` tab, you can see all your wallets and accounts organized as a 
tree. To the right of the tree, you can see details of the selected wallet 
or account.

The tree can be navigated in several ways. First of all, you can select any
entry by clicking on it with the mouse. You can also change the current 
selection with the keyboard. Up and Down keys will go to the previous and the 
next item respectively (including the `[ + Add wallet ]` button). Right key 
will go to the first child of the current item, and Left key will go to the 
parent. Ctrl-Up and Ctrl-Down can be used to jump between wallets: Ctrl-Up 
will first go to the current wallet (when an account is selected), then to 
the previous wallet. Ctrl-Down will always go to the next wallet. Finally, `h`,
`j`, `k`, `l` keys will work exactly as Left, Down, Up and Right arrow keys 
(without Ctrl).

To create a new wallet, select the `[ + Add wallet ]` line in the tree. 
A special widget will appear on the right. There you can either create 
a completely new wallet by specifying its name and passphrase, or restore 
an existing one. When a new wallet is created, Ariadne will print you its
mnemonic. **Please save it somewhere safe**.

Note that Ariadne mnemonics have more words than mnemonics of Daedalus (12), 
and that the last word of the Ariadne mnemonic is always fixed 
(currently it is `ariadne-v0`). This means you won't be able to use your 
Ariadne wallet in Daedalus. However, you can import your Daedalus wallet into 
Ariadne using the 12 word mnemonic.

When you select a wallet or an account, you can see its balance and send 
transactions using the form provided. You must fill at least one receiver's 
address. To add or remove address lines, use `[ - ]` / `[ + ]` buttons on
the right. If the wallet has a passphrase, you need to specify that as well.
When sending a transaction from a wallet, you can also choose which accounts 
to use as inputs using a list just above the form. Use Enter, Space or 
mouse click to select accounts.

When an account is selected, you will see the list of its addresses below 
the send form. If you focus an address, you can copy it to the clipboard by 
pressing Enter, Space, or clicking with the mouse. On macOS this will work out
of the box, on Linux you have to have either `xsel` or `xclip` installed. 
To make a new address, press the `[ Generate ]` button.

On the bottom half of the screen, you can see the REPL widget, which consists 
of command history and an input line. The REPL lets you do all sorts of things 
with your wallet using the `Knit` command language. In fact, all operations you
do using the widgets are mapped to `Knit` commands.

## `Knit` command language

`Knit` is a simple dynamically-typed command language created by Serokell. 
It lets you control Ariadne by calling and composing functions. In the `Help`
tab, you can see a complete list of all available functions, along with their 
signatures and short descriptions. You can also press Ctrl-Space to invoke code
completion. Let's explore `Knit` syntax and these functions in more detail.

### General syntax

To begin with, all `Knit` functions accept keyword arguments, some of which 
may be optional. Optional arguments are marked with a "`?`" suffix in help. 
If an argument can be specified many times (including zero), it's marked with 
a "`*`" suffix. If it can be specified many times, but has to be specified at
least once, it's marked with a "`+`" suffix. To specify a keyword argument, 
write its name, followed by `:` and a value. Types are checked while executing 
the command, not when parsing it. Functions may also have a variable number of 
arguments.

Let's take the `print` command for example. It expects a single argument named 
"`value`" of any type. To run it, type in the REPL:

    print value: "test"

Here we call the `print` command, passing it the string `"test"` as a value for 
the `"value"` argument. Of course, you can omit argument names, specifying them 
in the same order as in help:

    print "test"

You can also specify some of the command's optional arguments without 
specifying others. For example, `new-wallet` command creates a new wallet and 
expects two arguments: `name` and `entropy-size`. There are two ways to call it. 
You can call it with both the name and entropy size, omitting the argument names:

    new-wallet "test" 16

Or, if you want to use no name, but need to use an entropy size for the wallet, 
you can specify only the second argument:

    new-wallet entropy-size: 16

`Knit` language has literals of several formats:

- Strings in double quotes (`"string"`)
- Numbers with or without decimal point (`1`, `0.5`) or in scientific form (`1e-6`)
- ADA coin amounts (`1.5ADA`, see below)
- Cardano addresses without quotes (`sxtitePxjp5dcfm1u8gWgDBGMCEZMhGa6kUPu8VHhqpCtBDPExrJTTCCUHKkyEJSgjb41JT5Tfh1QXb7uUpgjyBKMw`)
- Task ids (`<1>`, see below)
- File paths, either relative or absolute, without quotes (`./file.txt`, `/etc/file.txt`)
- Hashes start with `#` (`#af879787af97b77c8866c655ddd666a77765565bbf98989898989898`). 
  For example, wallet identifiers (denoted as `WalletId` below) are hashes.

Let's first cover `Knit` commands that are not related to cryptocurrency. 
There are not many of them:

- `L`: create a list of things. Call it with any number of arguments, which may
   be of different types:

 ```
 L 1 2 3 "a" "b" "c"
 ```

- `help` and `logs`: switch to Help and Logs tabs respectively
- `print`: print an argument of any type
- `sleep`: delay execution for a given number of microseconds
- `not`, `true` and `false`: boolean negation and two boolean literals

Aside from that, you can enter multiple commands in one input. To do so, 
separate the commands with `;`.  REPL widget input line also allows you to 
break lines for visual stylization of commands. If you enter a backslash (`\ `) 
and press the Enter key, a new input line will appear.

All `Knit` commands are executed asynchronously, giving you back the control 
of REPL right after you press Enter. Ariadne has a task manager for these 
background commands. When you run a long-running task, its id 
(a number in angle brackets like `<1>`) and a string `"Waiting for result"` 
will appear. To cancel a command, use the `kill` command:

    kill <1>

You can also make some commands depend on others by using the `wait` command 
with a specific task id.

### User interface and Knit integration

Some of the `Knit` functions may access the internal state of Ariadne UI &mdash; 
namely, currently selected wallet or account. This allows you to omit the wallet 
name  when creating a new account using `new-account` command &mdash; it will 
use  the currently selected one.

### Cardano-related commands

Finally, the commands which will help you manage your `ADA` wallet. Each 
command will be annotated with its arguments and their types. Optional 
arguments will be marked with "`?`".

- `new-wallet name: String? entropy-size: Int?`: create a new wallet with a 
  name and size of entropy. Size of entropy influences the security of your 
  wallet and the length of the mnemonic. Default value is 16 bytes, leading 
  to mnemonics of 13 words. Ariadne mnemonics follow 
  [BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) 
  with an addition of one extra word as mentioned above.
  Created Wallet will also have a default auto generated account and address.
- `new-account wallet: WalletId or Word name: String?`: create a new account
  in the specified or currently selected wallet. You can specify a wallet 
  either by its ID or by its (zero-based) index in the wallet tree.
- `new-address wallet: WalletId or Word account: Word`: create a new address. 
  This command will use either the currently selected account or the one 
  specified by `wallet` and `account` arguments. Once again, you can either 
  use an ID or an index of a wallet.
  Account is refered by its index in the wallet tree.
- `balance`: get balance of the currently selected wallet or account.
- `select wallet: WalletId or Word a: Word?`: select an item. Specify a wallet 
  by its ID or index and then, optionally, specify an index of an acccount 
  in that wallet. Like this: `select 3 1` to select the second account in the 
  fourth wallet.
- `rename name: String`: give a new name to the currently selected wallet or account.
- `remove`: remove the currently selected wallet or account.
- `restore name: String? mnemonic: String full: Bool`: restore a wallet from 
  mnemonic. `full` argument specifies whether Ariadne should perform a full 
  restore (find all used accounts and addresses of the restored wallet in the 
  blockchain).
- `restore-from-daedalus-file name: String? file: FilePath full: Bool`: restore
  wallet from Daedalus's secret file. `full` argument has the same effect as 
  the `restore` command. Please note that the restored wallet will have the 
  same passphrase as in Daedalus.

There is also a command to send a transaction from your wallet:

    send wallet: (String or Word)? account: (String or Word)* policy: InputSelectionPolicy? out: TxOut+

Since one transaction can have multiple outputs, you have to pass these outputs 
to the `send` command. An output is constructed with the `tx-out` command, 
which takes a receiver's address and an amount of ADA you want to send to this
address. There are several ways to specify the amount of coins:

- Just a number, like `2` or `0.5`. It will be treated as an amount of ADA 
   coins. Note that you can't have a fraction of ADA less than 0.000001 (10⁻⁶).
- A number with `ADA` suffix: `2ADA`, `0.5ADA`. This option is identical to the 
  previous one.
- A number with `Lovelace` suffix: `100Lovelace`, `500000Lovelace`. One 
  Lovelace is 0.000001 (10⁻⁶) of one ADA. Lovelaces can't be fractional.

For your convenience, `ADA` and `Lovelace` suffixes are case-insensitive.

So, to construct an output, do this:

    tx-out sxtitePxjp5dcfm1u8gWgDBGMCEZMhGa6kUPu8VHhqpCtBDPExrJTTCCUHKkyEJSgjb41JT5Tfh1QXb7uUpgjyBKMw 2ADA

Note that the receiver's address does not take quotes.

Finally, send a transaction by giving the `send` command a list of outputs 
(`out` argument), and optionally wallet's name or index (`wallet` argument) as 
well as a list of source accounts (as names or indices). If you don't specify a 
wallet, the currently selected one will be used.

Transaction inputs are selected automatically from a set of accounts that 
depends on the `account` arguments and the current selection:
* If at least one account is explicitly specified, only the specified accounts 
  can be used as inputs.
* If no accounts are explicitly specified, and an account from the input wallet 
  is selected, only its addresses will be used as inputs. Otherwise, addresses 
  will be picked from all accounts in the input wallet.

Input selection algorithm depends on the `policy` argument, which can
be one of the following:
* `security`. In this case, TODO
* `high-throughput`. In this case, confirmed inputs will be prefered.

By default, the `security` policy is used.

Here's an example:

```
send out:(tx-out sxtitePxjp5dcfm1u8gWgDBGMCEZMhGa6kUPu8VHhqpCtBDPExrJTTCCUHKkyEJSgjb41JT5Tfh1QXb7uUpgjyBKMw 2ADA)
     out:(tx-out sxtitePxjp65TKMXHNaLsBbJywqdYW4xLJzNVvT7ksTvVR1AVxFTH8PivZa2VtfcD9bu62MWKu6dnjbreSZCdsuDB1 100000Lovelace)
```

As a call to the `send` command can get rather long, it's a good idea to use 
multiline feature to write each output on a separate line.

If a transaction is successful, after a while you'll get its id. If there is a 
problem, e.g. if you try to send more coins than you have, an error will be 
printed.
