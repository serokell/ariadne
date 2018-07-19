Ariadne Style Guide
============================
> This style guide is mostly a copy of
> [Serokell Haskell Style Guide](https://github.com/serokell/serokell-util/blob/master/serokell-style.md)
> with some modifications.
> This style guide's aims are code beauty, readability and understandability.

You can find our other formatting utilites and guidelines which expand the code style:
* [.stylish-haskell.yaml](https://github.com/serokell/serokell-util/blob/master/.stylish-haskell.yaml)
* [Universum](https://github.com/serokell/universum)
* [Custom _HLint_ rules](https://github.com/input-output-hk/cardano-sl/blob/master/HLint.hs)

General guide lines
-------------------
### Line Length

Maximum line length is *80 characters* or *100 characters* if necessary.

Modern screens have high definition and big width.
But with some tiling managers and two terminals on one screen you are not able to
see many characters on one line.
On the other hand, restricting line size to a very small number like 80 leads to
some crazy indentation despite the fact that
shorter lines should force you to write well structured code.
That's why *100* is a reasonable compromize.


### Indentation

Tabs are illegal.  Use spaces for indenting.  Indent your code blocks
with *2 or 4 spaces*.  Indent the `where` keyword with two spaces to set it
apart from the rest of the code and indent the definitions in a
`where` clause with 2 spaces.  Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### Blank Lines

One blank line between top-level definitions. No blank lines between
type signatures and function definitions. Add one blank line between
functions in a type class instance declaration if the function bodies
are large. You can add blank lines inside a big `do` block to separate logical
parts of it. You can also use blank lines to separate definitions inside `where`
clause. Blank lines are used to separate imports (this is described in section below).

### Whitespace

Surround binary operators with a single space on either side. In case of
currying add one space between the argument and the operation.

_Exceptions (where you're allowed to not follow this rule)_:

1. `%` operator for string formatting from `Formatting` library. Compare:

`("block with "%int%" entries") cnt`

`("block with " % int % " entries") cnt`

2. `+|`-like operators from [`fmt`](http://hackage.haskell.org/package/fmt) library. Compare

`"block with "+|cnt|+" entries"`

`"block with " +| cnt |+ " entries"`

So, you can omit spaces if surrounding strings have spaces. It also helps in
cases when formatted strings are too long.

Use some tool to remove trailing spaces!

### Naming convention

Casing:
+ **_lowerCamelCase_** for function and variable names.
+ **_UpperCamelCase_** for types.
+ TODO: some convention for global constants?

Don't use short names like `n`, `sk`, `f` unless their meaning is clear from
context (function name, types, other variables, etc.).

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: two or three letter abbreviations, e.g. `IO`, `STM`.

**Records name conventions**

If data type has only one constructor then this data type name should be same
as constructor name (also applies to `newtype`).

```haskell
data User = User Int String
```

Field name for `newtype` should start with `un` or `run` prefix followed by type name.

* `run` for wrappers with monadic semantic.
* `un` for wrappers introduced for type safety.

Motivated by [this discussion](https://www.reddit.com/r/haskell/comments/7rl9hx/newtype_field_naming_getx_vs_runx/).

```haskell
newtype Coin = Coin { unCoin :: Int }
newtype PureDHT a = PureDHT { runPureDHT :: State (Set NodeId) a }
```

Field names for record data type should start with every capital letter in type
name.

```haskell
data NetworkConfig = NetworkConfig
    { ncDelay :: Microsecond  -- `nc` corresponds to `_N_etwork_C_onfig`
    , ncPort  :: Word
    }
```

TODO: consider remove this rule? GHC has `-XOverloadedRecordFields` with
[`HasField`](https://www.stackage.org/haddock/lts-10.4/base-4.10.1.0/GHC-Records.html#t:HasField)
type class.

**Library specific conventions**

Add `F` suffix to custom formatters to avoid name conflicts:

```haskell
nodeF :: NodeId -> Builder
nodeF = build
```

### Comments

#### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

#### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions, the documentation should give enough information to
apply the function without looking at its definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments, format them this way:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

#### End-of-Line Comments

Separate end-of-line comments from the code with 2 spaces. Align
comments for data type definitions. Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * n + 9
  where
    salt = 453645243  -- Magic hash salt.
```

#### Links

Use in-line links economically.  You are encouraged to add links for
API names. It is not necessary to add links for all API names in a
Haddock comment. We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your opinion), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Top-down guideline
------------------

### LANGUAGE extensions section

Write each `LANGUAGE` pragma on its own line, sort them alphabetically.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
```

### Module name

Use singular when naming modules (e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`). Sometimes it's acceptable to use plural
(e. g. `Types`, `Instances`).

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
       ( -- * The @Set@ type
         Set
       , empty
       , singleton

         -- * Querying
       , member
       ) where
```

Some clarifications:

1. Use 7 spaces indentation for export list (so that bracket is below
   the first letter in module name).
2. You can split export list into sections or just write all as single section.
3. It is strongly adviced to sort each section alpabetically. However,
   classes, data types and type aliases should be written before
   functions.

### Imports

Imports should be grouped in the following order:

0. Implicit import of custom prelude (for example [`universum`](https://github.com/serokell/universum)) if used.
1. Everything from hackage packages or from your packages outside current project.
2. Everything from current project.
3. Everything from current target (like `Bench.*` or `Test.*`).
4. Qualified imports

Put a blank line between each group of imports.

The imports in each group should be sorted alphabetically, by module name.

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a =
    Branch !a !(Tree a) !(Tree a)
  | Leaf
```

The following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

If you have record with multiple constructors (which is generally bad idea
because you getters become partial functions but okay if you use
`-XRecordWildCards`) then align curly braces with
shift to the constructor name but } should go in the end of last field.

```haskell
data Address
    = PubKeyAddress
        { addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress
        { addrScriptHash   :: !(AddressHash Script)
        , addrDistribution :: ![(AddressHash PublicKey, Coin)] }
    deriving (Show, Eq)
```

If there is only one field for every constructor more compact style is allowed.

```haskell
data Address
    = PubKeyAddress { addrKeyHash    :: !(AddressHash PublicKey) }
    | ScriptAddress { addrScriptHash :: !(AddressHash Script)    }
    deriving (Show, Eq)
```

Type classes in `deriving` section should be always surrounded by
parentheses. Space between names is optional.

_WARNING_: try to avoid aggressive autoderiving. Deriving instances can
slowdown compilation
(stated here: http://www.stephendiehl.com/posts/production.html)

> Deriving instances of Read/Show/Data/Generic for largely recursive ADTs can
> sometimes lead to quadratic memory behavior when the nesting gets deep.

If you're using GHC-8.2.2 or higher you should use
[`-XDerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies)
extension and specify the way you derive explicitly. Like this (see
[this question](https://github.com/jaspervdj/stylish-haskell/issues/186) for example):

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

newtype SpecialId = SpecialId Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype Read
    deriving anyclass (FromJSON, ToJSON)
```

### Function declaration

All top-level functions _must_ have type signatures.

All functions inside `where` _should_ have type signatures. Explicit type
signatures help to avoid cryptic type errors. Though when working with pure
arithmetics and everything is `Integer` then type signatures can look cumbersome.

> You most likely need `-XExplicitForAll` and `-XScopedTypeVariables` extensions
> to write polymorphic types of functions inside `where`.

Specialize function type signature for concrete types if you're using this function
with only one type for each argument. Otherwize you should use more polymorphic
version. Compiler can optimize specialized functions better
(TODO: link to haskell manual) and meaning of this function may be clearer.
Use this rule unless you are the library creator and want your library to be abstract
as possible.

It is allowed to omit parentheses for only one type class constraint.

If function type signature is very long then place type of each argument under
its own line with respect to alignment.

```haskell
putValueInState
    :: MonadIO m
    => UserState
    -> Maybe Int
    -> AppConfig
    -> (Int -> m ())
    -> m ()
```

If the line with argument names is too big then put each argument on its own line
and separate it somehow from body section.

```haskell
putValueInState
    userState
    mValue@(Just x)
    Config{..}        -- { should go after ctor name without space
    valueModificator
  = do
    <code goes here>
```

In other cases place `=` sign on the same line where function definition is.

Use `() <$` to  ignore the result of function. It looks cool. If this is not possible
due to `$` then use either `_ <-` or `void $`.

```haskell
foo = do
    _  <- forkIO $ myThread config  -- can't be used as last statement
    () <$ sendTransactionAndReport 1 "tx42"
```

Put operator fixity before operator signature:

```haskell
-- | This operator looks cool!
infixl 5 />
(/>) :: Uri -> PathPiece -> Uri
```

### Pragmas

Put pragmas immediately following the function they apply to.
Example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In case of data type definitions you must put the pragma before
the type it applies to. Example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

### Hanging Lambdas

Don't insert a space after a lambda.

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over _if-then-else_
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

You can write _if-then-else_ in imperative style inside do blocks

```haskell
foo = do
    someCode
    if condition then do
        someMoreCode
        andMore
    else
        return ()
```

Use `-XMultiwayIf` only if you need complex `if-then-else` inside `do`-blocks.

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

Align the `->` arrows when it helps readability.

It's suggested to use `-XLambdaCase` extension because it's a great extension!
See [this discussion](https://github.com/jaspervdj/stylish-haskell/issues/186) for nice usage examples.

### ApplicativeDo

Don't use `-XApplicativeDo` blindly everywhere. Only when necessary.

You _should_ use `-XApplicativeDo` when you're writing CLI-arguments parser with
`optparse-applicative` because it's very easy to mess up with arguments order.
Though, be aware of [some non-obvious behavior](https://www.reddit.com/r/haskell/comments/7679g8/ghc_821_applicativedo_possible_bug/)
when you're using pattern-matching inside `do`-blocks.

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy.  This helps to avoid many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file.  Including this flag in the file itself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Misc
----

### Point-free style ###

Avoid over-using point-free style.  For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

Cabal file formatting
---------------------

### Modules & libraries

Modules and libraries should go in alphabetical order inside corresponding
sections. You can put blank lines between groups in each section.

### Warnings ###

Code should be compilable with `-Wall` without warnings.

Some warnings can be disabled on per-module basis if there is a good
reason for it.
