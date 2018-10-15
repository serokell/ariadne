# Knit – an Extensible Command Language

Knit is a framework to assemble domain-specific command languages from
_components_. Each component defines its own lexical rules, types, built-in
commands, pretty-printing routines, etc. Although Knit is not a single language
but a family of languages parametrized by a list of components, we will say "the
Knit language" to refer to instantiations of it.

As a command language, it exhibits the following properties:

* imperative with side effects
* strict evaluation
* dynamically typed

We're looking to make it statically typed in the future.

## The Pipeline

The user enters commands as text and gets their results as text. What happens in
between is the Knit pipeline, illustrated below:

```
         Text
          |
          v
   +---------------+
   |   Tokenizer   |
   +---------------+
          |
          | [Token]
          |
          v
   +--------------+
   |    Parser    |
   +--------------+
          |
          | Expr Name
          |
          v
 +--------------------+
 |  Name  Resolution  |
 +--------------------+
          |
          | Expr CommandProc
          |
          v
 +-------------------+
 |  Evaluation/Exec  |
 +-------------------+
          |
          | Value
          |
          v
   +-------------+
   |  Inflation  |
   +-------------+
          |
          | Expr Name
          |
          v
 +-----------------+
 |  Pretty-Printer |
 +-----------------+
          |
          v
         Text
```

Let's say the user input is the following string of characters:

```
"mk 157 obj:(open ./tmp)"
```

The first step of processing user input is tokenization – grouping sequences of
characters into tokens. The result of tokenization might look like this
(pseudocode):

```
[ TokenName "mk",
  TokenNumber 157,
  TokenKey "obj",
  TokenParen LeftSide,
  TokenName "open",
  TokenFilePath "./tmp",
  TokenParen RightSide
]
```

The tokenization step is distinct from parsing: it can never fail, and it
produces a flat sequence of tokens as its result, not a tree-like structure.

The parser takes the token sequence and builds an abstract syntax tree. The
resulting AST might look like this (pseudocode):

```
ProcCall
  { command = "mk",
    arguments =
      [ ArgPos { value = LitNumber 157 },
        ArgKw
          { key = "obj",
            value =
              ProcCall
                { command = "open",
                  arguments = [ ArgPos { value = LitFilePath "./tmp" } ]
                }
          }
      ]
  }
```

The abstract syntax tree is represented by a recursive data type that supports
procedure calls with positional and keyword arguments, and also literals.

At this point commands are represented by their names. The next step – name
resolution – is to look up these names in a table of command definitions, so
that instead of `command = "mk"` we will have something like this (pseudocode):

```
command =
  CommandProc
    { name = "mk",
      argumentPrepare = id,
      argumentConsumer = ...,
      repr = ...,
      help = "make a thing"
    }
```

Finally, when tokenization, parsing, and name resolution are done, we can
evaluate/execute the command. This might or might not require `IO`, depending on
what Knit components are used, but in the end it will result in a value like
this (pseudocode):

```
ValueBool True
```

In order to show the resulting value to the user, we first convert them to the
smallest expression that would evaluate to this value. This step is called
inflation. For instance, the boolean value `True` can be constructed by calling
the `"true"` command:

```
ProcCall "true" []
```

Finally, we run the pretty-printer on this expression to convert it to a textual
form:

```
"true"
```

## The Component System

The component system of Knit allows extending each step of the pipeline. Here's
a checklist of data families and classes, instances of which are needed to make
a component work, in no particular order:

* `ComponentValue` – type for runtime values manipulated by Knit programs
* `ComponentLit` – type for AST nodes representing values
* `ComponentToken` – type for tokens
* `ComponentTokenizer` – methods for tokenization
* `ComponentDetokenizer` – methods for the inverse of tokenization
* `ComponentTokenToLit` – methods for mapping tokens to literals
* `ComponentPrinter` – methods pretty-printing literals and token names
* `ComponentCommandProcs` – a list of all commands added by the component
* `ComponentCommandRepr` – type for representing the commands
* `ComponentCommandExec` – methods for evaluation/execution of commands
* `ComponentLitToValue` – methods for mapping literals to values
* `ComponentExecContext` – type for evaluation/execution context
* `ComponentInflate` – methods for mapping values to expressions

Understanding the purpose of each might require a good understanding of the
internal Knit architecture, but we only need to know how to define each
instance, not why, and this should be doable. We are going to familiarize
ourselves with these data families and classes by means of a case study.

Let's say we want to define a component for task management – the system can
spawn tasks, and the user can use Knit to `kill` a task or `wait` for its
result:

```
wait <78>; print "task 78 has completed"
```

This means that the task management component of Knit must add the following
features to the language:

* a new value type for task identifiers
* the `<nn>` syntax for task identifier values
* the `kill` and `wait` commands

Assume we already have the task manager itself implemented and exposed with
the following interface:

```
data TaskId = TaskId Natural
  deriving (Eq, Ord, Show)

data TaskManagerFace v =
  TaskManagerFace
    { spawnTask :: (TaskId -> IO v) -> IO TaskId,
      lookupTask :: TaskId -> IO (Maybe (Async v))
    }
```

Spawning is done elsewhere in the system, – we will only use `lookupTask` to
get our hands on an `Async`, and then either `Async.wait` or `Async.cancel`.

We start writing the Knit bindings to the task manager by defining a component
identifier:

```
data TaskManager
```

This is an uninhabited, empty data type. It serves merely as a type-level tag, a
marker, a name. We'll never work with its values, because there are none.
However, we are about to declare data family instances and class instances for
it – these definitions are what makes a Knit component.

We start with a data instance for the `ComponentValue` data family:

```
data instance ComponentValue _ TaskManager
  = ValueTaskId TaskId
  deriving (Eq, Show, Ord)

makePrisms 'ValueTaskId
```

This means that now the Knit language can manipulate values represented by the
`TaskId` type.

Compare that to the `ComponentValue` data instance for the `Core` component:

```
data instance ComponentValue components Core
  = ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [Value components]

makePrisms 'ValueBool
```

In the `ComponentValue` instance for the `Core` component, we have booleans,
numbers, strings, units, filepaths, and lists. Notice that we can reference
other components here: the `ValueList` constructor defined in the `Core`
component can contain `ValueTaskId` elements defined in the `TaskManager`
component.

We achieve this by defining `ComponentValue` and `Value` like this:

```
data family ComponentValue components component

newtype Value components =
  Value { getValueUnion :: Union (ComponentValue components) components }
```

So what we have here is a neat application of mutually recursive data types.
`Value` is defined as a union of `ComponentValue` instances for each component,
while each components' data instance for `ComponentValue` can mention this
entire `Value` type (as the `ValueList` constructor does).

A good intuition for `Union` is that it's a bit like concatenating the
constructors of all its element types. So for `Value '[Core, TaskManager]` we
are going to have the following rough equivalence:

```
----------------------------------
-- unwrapping the 'Value' newtype:
----------------------------------

type Value '[Core, TaskManager] ≈
  Union (ComponentValue '[Core, TaskManager]) '[Core, TaskManager]

---------------------------------------
-- intuitive interpretation of 'Union':
---------------------------------------

type Value '[Core, TaskManager] ≈
    ComponentValue '[Core, TaskManager] Core
  | ComponentValue '[Core, TaskManager] TaskManager

-------------------------------------
-- inline data instance constructors:
-------------------------------------

type Value '[Core, TaskManager] ≈
    ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [Value '[Core, TaskManager]]
  | ValueTaskId TaskId
```

Treating `Union` this way means that we can think of `Value` as an extensible
data type – individual components can add constructors to it. Of course, if we
were to talk about this rigorously, Haskell does not have extensible data types,
but this is one way to model them.

Now let's pay a bit more attention to these lines:

```
makePrisms 'ValueTaskId
makePrisms 'ValueBool
```

Here we're using an exotic feature of the `lens` package to generate prisms for
data instances. Usually, when one needs to generate prisms, he would mention
the data type name:

```
makePrisms ''Either -- would generate the `_Left` and `_Right` prisms
```

However, in case of data instances, we have to reference the instance by one of
the value constructors. Notice that we have `'ValueBool` with a single prime in
front of it, not with two primes – this is Template Haskell syntax for
constructor names, as opposed to type names. So `makePrisms 'ValueBool` will
generate prisms `_ValueBool`, `_ValueNumber`, `_ValueString`, etc.

Moving on to the other parts of a component definition, in addition to
`ComponentValue` which represents values manipulated during command execution,
we have `ComponentToken` for tokenization results and `ComponentLit` for
abstract syntax tree nodes:

```
data instance ComponentLit TaskManager
  = LitTaskId TaskId
  deriving (Eq, Show, Ord)

data instance ComponentToken TaskManager
  = TokenTaskId TaskId

makePrisms 'TokenTaskId
```

During tokenization, we want to map `<nn>` into `TokenTaskId`:

```
instance Elem components TaskManager => ComponentTokenizer components TaskManager where
  componentTokenizer = [ toToken . TokenTaskId <$> pTaskId ]
    where
      pTaskId :: Tokenizer TaskId
      pTaskId = do
        void $ P.char '<'
        cid <- P.decimal
        void $ P.char '>'
        return $ TaskId cid
```

We extend the tokenization process by defining an instance of the
`ComponentTokenizer` class. In its context, we have the `Elem components
TaskManager` constraint – it might seem redundant, but without it GHC can't
assume that `TaskManager` is one of the components we have, and we need this
assumption to construct a token with `toToken`.

Here is, for comparison, the `ComponentTokenizer` instance for `Core`:

```
instance Elem components Core => ComponentTokenizer components Core where
  componentTokenizer =
      [ toToken . TokenNumber <$> pScientific
      , toToken . TokenString <$> pString
      , toToken . TokenFilePath <$> pFilePath
      ]
    where
      pScientific :: Tokenizer Scientific
      pString :: Tokenizer String
      pFilePath :: Tokenizer FilePath
      ...
```

Tokenization is an invertible process, so we have a class for the opposite
operation, `ComponentDetokenizer`:

```
instance ComponentDetokenizer TaskManager where
  componentTokenRender = \case
    TokenTaskId (TaskId cid) -> sformat ("<"%build%">") cid
```

After tokenization, we have parsing – as of now, Knit doesn't explicitly support
arbitrary extensions to the grammar, but that's mainly because there was no need
for it so far, as custom tokens cover most of the needs. So for now, all we can
do at the parsing step is to map our custom tokens into literals:

```
instance Elem components TaskManager => ComponentTokenToLit components TaskManager where
  componentTokenToLit = rule $
    toLit . LitTaskId <$> tok (_Token . uprismElem . _TokenTaskId)
```

For the inflation step of the pipeline, we have to define how `ValueTaskId` can
be transformed into an expression, and that's where we use `LitTaskId`:

```
instance Elem components TaskManager => ComponentInflate components TaskManager where
  componentInflate (ValueTaskId cid) =
    ExprLit $ toLit (LitTaskId cid)
```

One of the things needed for evaluation is a mapping from literals to values:

```
instance ComponentLitToValue components TaskManager where
  componentLitToValue = \case
    LitTaskId cid -> ValueTaskId cid
```

Nothing complicated or remarkable so far. In fact, someone would consider all
these conversions to be boilerplate – in case of the task manager,
`ComponentToken`, `ComponentLit`, and `ComponentValue` are basically the same.
It isn't necessarily the case for other components, though.

In any case, we are about to implement the `kill` and `wait` commands, and this
is going to be more involved. The first thing to consider is how each command
should be represented. For `kill` and `wait`, a reasonable representation is an
`IO` action that takes `TaskManagerFace` (which we defined earlier) as input:

```
data instance ComponentCommandRepr components TaskManager
  = CommandAction (TaskManagerFace (Value components) -> IO (Value components))
```

Now that we decided how commands of the `TaskManager` component are represented,
we will create a list of them:

```
instance
  ( AllConstrained (Elem components) '[TaskManager, Core]
  ) => ComponentCommandProcs components TaskManager
  where
    componentCommandProcs =
      [ CommandProc
          { cpName = "kill"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction $ \TaskManagerFace{..} -> do
              mTask <- lookupTask tid
              whenJust mTask cancel
              return . toValue $ ValueUnit
          , cpHelp = "kill a task with a specified id"
          }
      , CommandProc
          { cpName = "wait"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction $ \TaskManagerFace{..} -> do
              mTask <- lookupTask tid
              case mTask of
                Nothing -> throwIO NoTaskException
                Just task -> wait task
          , cpHelp = "wait for a specific task to finish"
          }
      ]
```

Here we have a list where each element is a command implementation –
`CommandProc` – that contains the following fields:

* `cpName` – the name of the command
* `cpArgumentPrepare` – preprocessor for the command arguments (`identity` means
  no preprocessing)
* `cpArgumentConsumer` – definition of what arguments the command expects
  (`getArg tyTaskId "id"` means the command expects a single argument named `id`
  represented as `ValueTaskId`)
* `cpRepr` is a function from the structure produced by `cpArgumentConsumer`
  (for example, `tid :: TaskId`) to the command implementation
* `cpHelp` is a textual description of the command

Those `CommandProc` are what the name resolution stage of the pipeline finds and
inserts into the AST instead of bare command names.

`tyTaskId` is a type projection, it describes what values will be accepted as
command arguments, and which ones will not. For instance, `wait id:<1>` is a
valid command invocation, but `wait id:"hello"` isn't. Here's how we define it:

```
tyTaskId :: Elem components TaskManager => TyProjection components TaskId
tyTaskId = TyProjection "TaskId" (preview _ValueTaskId <=< fromValue)
```

In the `Core` component, we have type projections for other value types:

```
tyBool :: Elem components Core => TyProjection components Bool
tyBool = TyProjection "Bool" (preview _ValueBool <=< fromValue)

tyFilePath :: Elem components Core => TyProjection components FilePath
tyFilePath = TyProjection "FilePath" (preview _ValueFilePath <=< fromValue)

tyString :: Elem components Core => TyProjection components Text
tyString = TyProjection "String" (preview _ValueString <=< fromValue)
```

So, for instance, if we wanted an argument of the boolean type and named `flag`,
we'd write `getArg tyBool "flag"`. If a command requires several arguments, we
can use the `Applicative` interface to combine several `getArg`:

```
cpArgumentConsumer =
  (,,) <$>
    getArg tyA "a" <*>
    getArg tyB "b" <*>
    getArg tyC "c"
```

There's also support for optional arguments and variadic functions. While
`getArg` expects exactly one argument, there's `getArgOpt` that expects zero or
one, `getArgMany` that expects zero or more, and `getArgSome` that expects one
or more.

With the commands defined, we need to think how to execute them. First, what
context do our commands need? Since they require `TaskManagerFace` as input,
that is also our execution context:

```
newtype instance ComponentExecContext _ components TaskManager =
  TaskManagerExecCtx (TaskManagerFace (Value components))
```

Second, given a command and an execution context, how do we execute this
command? Since we represent our commands as `IO` actions, we need the execution
monad to support the `MonadIO` interface. Other than that, we simply supply the
`TaskManagerFace` from the execution context to the command, and run the
resulting `IO` action:

```
instance MonadIO m => ComponentCommandExec m components TaskManager where
  componentCommandExec (TaskManagerExecCtx face) = \case
    CommandAction action -> liftIO $ action face
```

And that's it – now we have a working `TaskManager` component.
