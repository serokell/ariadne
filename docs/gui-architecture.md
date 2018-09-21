# Ariadne Qt GUI

`ariadne-qt` uses [qtah](https://gitlab.com/khumba/qtah) library to build the UI. This doc assumes that
the reader has some experience with Qt in other languages.

## Modules overview

GUI resides in [ui/qt-lib](../ui/qt-lib) folder. Main entry point is
[Ariadne.UI.Qt](../ui/qt-lib/src/Ariadne/UI/Qt.hs) module that creates `QApplication` and calls
[Ariadne.UI.Qt.MainWindow](../ui/qt-lib/src/Ariadne/UI/Qt/MainWindow.hs) to create all the widgets. `MainWindow`
module calls `init*` functions for other widgets, which do the same in turn, recursing down the widget tree.

Each widget is contained in one module, for example [Ariadne.UI.Qt.Widgets.TopBar](../ui/qt-lib/src/Ariadne/UI/Qt/Widgets/TopBar.hs)
creates everything related to the top bar (ariadne logo, repl button and so on). A widget is represented by
a record that contains everything needed to operate it in the run time: `Qt` widget objects, auxillary data,
child widgets' records and sometimes `IORef`s. Widgets are supposed to leave in
`type UI widget = ReaderT widget IO` monad, so that one doesn't have to pass widget record explicitly.
This allows to call child widget functions using `magnify` from lenses.

## Typical widget

Typical widget has something like this code:

```haskell
data Widget =
  Widget
    { widget :: QWidget.QWidget
    , someEdit :: QLineEdit.QLineEdit
    , someButton :: QPushButton.QPushButton
    }

makeLensesWith postfixLFields 'Widget

initWidget :: IO (QWidget.QWidget, Widget)
initWidget = do
  widget <- QWidget.new

  -- create someEdit, someButton, add them to qwidget

  let wgt = Widget{..}

  connect_ someButton QAbstractButton.clickedSignal $ \_ -> someButtonClicked wgt

  return (widget, wgt)

someButtonClicked :: Widget -> IO ()
someButtonClicked Widget{..} = do
  -- do something
```

Of course, `Widget` must be replaced with concrete widget name. `initWidget` can take additional
arguments, most commonly `UiLangFace`, and either store it in partially applied signal handlers or in the
widget record.

Widgets can send events to their parents. For this, export function like this:

```haskell
doOnButtonClicked :: IO () -> UI Widget ()
doOnButtonClicked handler = do
  button <- view someButtonL
  liftIO $ connect_ button QAbstractButton.clickedSignal $ \_ -> handler
```

Then parent widget can connect to that event by doing this in `UI ParentWidget` monad:

```haskell
  magnify childWidgetL $ doOnButtonClicked someHandler
```

`someHandler` can also be another widget's `UI` action. In that case you can use `runUI handler =<< view otherWidgetL`.

## Creating dialogs

Dialogs are stored in [Ariadne.UI.Qt.Widgets.Dialogs](../ui/qt-lib/src/Ariadne/UI/Qt/Widgets/Dialogs) module
and follow similar structure:

1. dialog record, containing `QDialog.QDialog` object
2. init function, that creates layout for the dialog and returns the record
3. specific dialog result data type, that is returned to caller
3. run function, that runs the dialog modally:
```haskell
runDialog :: IO DialogResult
runDialog = do
  dlg@Dialog{dialog = dialog} <- initDialog
  result <- toEnum <$> QDialog.exec dialog
  valid <- isValid del

  return $ case result of
    QDialog.Accepted -> if valid then DialogSuccess else DialogFailure
    QDialog.Rejected -> DialogFailure
```
4. validation function `isValid :: Dialog -> IO Bool` that checks that user input is correct
5. `revalidate :: Dialog -> IO ()` function that is connected to every input's changed signal and enables
or disables dialog buttons accordingly

Some dialogs need to return data. In that case make a function `fillDialogData :: Dialog -> IO (Maybe DialogResult)`
and define `isValid dlg` as `isJust <$> fillDialogData dlg`.

Some common dialog elements are collected in [Ariadne.UI.Qt.Widgets.Dialogs.Util](../ui/qt-lib/src/Ariadne/UI/Qt/Widgets/Dialogs/Util.hs).
These include:

- `createLayout :: (QWidget.QWidgetPtr wgt) => wgt -> IO QVBoxLayout.QVBoxLayout`: creates VBox layout for the
  dialog with correct margins and spacing
- `addHeader :: (QWidget.QWidgetPtr hdr) => QVBoxLayout.QVBoxLayout -> hdr -> IO ()`: adds header widget (`QLabel`)
  to the dialog, styling it correctly
- `createRowLayout :: IO QHBoxLayout.QHBoxLayout`: creates HBox layout for the dialog row
- `addRow :: (QWidget.QWidgetPtr lbl, QWidget.QWidgetPtr wgt) => QVBoxLayout.QVBoxLayout -> lbl -> wgt -> IO ()`:
  adds label and widget to main dialog VBox layout, setting correct strecth factors
- `addRowLayout :: (QWidget.QWidgetPtr lbl, QLayout.QLayoutPtr lay) => QVBoxLayout.QVBoxLayout -> lbl -> lay -> IO ()`:
  same as above, but adds custom layout instead of simple widget
- `addSeparator :: QVBoxLayout.QVBoxLayout -> IO ()`: adds separator line
- `createSubWidget :: IO (QWidget.QWidget, QVBoxLayout.QVBoxLayout)`: create `QWidget` that can be used as
  a part of the dialog. Useful when a part must be hidden sometimes. Add this widget to VBox with `QVBoxLayout.addWidget`.
- `createCheckBoxWithLabel :: Text -> IO (QCheckBox.QCheckBox, QLabel.QLabel)`: creates a checkbox with an
  associated label, so that click on label will toggle the checkbox
- `createCheckBox :: QVBoxLayout.QVBoxLayout -> Text -> IO QCheckBox.QCheckBox`: same as above, but also
  adds that checkbox and label to VBox, checkbox left of the label

You need to connect main dialog button to handler that calls `QDialog.accept` (or `QDialog.reject` for cancel button).
This will return to `runDialog` function, providing it Qt dialog result (Accepted or Rejected).

If you want non-modal window, use `QWidget.show` instead of `QDialog.exec`. For example take a look at
[Ariadne.UI.Qt.Widgets.Dialogs.Request](../ui/qt-lib/src/Ariadne/UI/Qt/Widgets/Dialogs/Request.hs).

## Interacting with the backend

Backend communication has two sides: calling backend from UI and getting results from it.

### Calling backend functions in GUI

There are two ways to ask backend to do something. The primary one is to construct Knit commands.
The other, more easy one, is for simple, fast and often pure(ish) actions like validating address
format.

Either way requires passing a special record from the root widget to your one. For simple actions
you need to pass along `UiWalletFace` defined in
[Ariadne.UI.Qt.Face](../ui/qt-lib/src/Ariadne/UI/Qt/Face.hs). This record contains functions that
actually call backend. These functions are filled in `Main`. To add your own functions you first
need to add them to `WalletUIFace` in
[Ariadne.Wallet.Face](../ariadne/cardano/src/Ariadne/Wallet/Face.hs), add actual implementations
there in [MainTemplate.hs](../ariadne/cardano/src/Ariadne/MainTemplate.hs) and put them into
`UiWalletFace` in the `Main.hs` module.

However, most backend actions are (and should be) Knit commands. To call them you need to have
`UiLangFace` record in your widget. This record defines `langPutUiCommand` function, which accepts
`UiCommand` type defined in the same [Ariadne.UI.Qt.Face](../ui/qt-lib/src/Ariadne/UI/Qt/Face.hs)
module. These UI-specific commands are then mapped into actual Knit commands in the `Glue.hs` module
by the `opToExpr` function defined there. To add new `UiCommand` you must encode it's parameters
into corresponding Knit expression using existing commands as example. More info about `Knit` can be
found in the [Knit doc](../knit/README.md).

`langPutUiCommand` returns `Either Text UiCommandId`. Text (in `Left`) will contain error message if
something goes wrong. Usually you need to display that message using `QMessageBox.critical`.
`UiCommandId` (in `Right`) is id of your command. It can be used to match results of many similar
commands later.

### Getting results from the backend

`MainWindow.hs` module defines `handleMainWindowEvent` event function that is responsible for handling
all events coming from the backend. Events are sent asynchronously and are represented by `UiEvent`
type, defined once again in `Face`. `handleMainWindowEvent` translates these events into
widget-specific events and calls widgets' `handle*` function, using `magnify`, since widgets'
handlers are `UI SomeWidget ()` actions.

Each widget should define the type of events it wants to handle. Most events are results returned by
commands sent earlier, however some of them, like backend status update event, are completely
independent from GUI actions.

To add a new command that should return a result, add that result to `UiCommandResult` data type in
`Face` and handle it in `resultToUI` in `Glue`. That function has two parameters &mdash; the result
sent by backend and the actual command that led to this result. It then must turn result into
correct UI type depending on the command.

## Debugging with GammaRay

There is a very useful tool for debugging GUI problems: [GammaRay](https://www.kdab.com/development-resources/qt-tools/gammaray/).
It is like Chrome Dev Tools, but for Qt. It allows to see every `QWidget` living in the application, view and
dynamically change their properties, measure distances, pick colors and more.

The easiest way to install it is to build from source:

1. Clone https://github.com/KDAB/GammaRay
2. `cd` into that repo, `mkdir build`, `cd build`
3. Run `cmake ..` If you have all the needed dependencies and compilers, it should succeed. You need to have
   installed from your distro's repos `cmake`, `qt` dev package and `gdb`. Most likely you already have all
   of this if you are able to build `ariadne-qt`.
4. Run `make` to build it
5. And finally, launch GammaRay: `./bin/gammaray`

You can refer to [their wiki](https://github.com/KDAB/GammaRay/wiki/Getting-GammaRay) for more instructions.

Launch `ariadne-qt` first, then `GammaRay`. After launching GammaRay you will see a list of all running Qt
processes in your system. Double click on `ariadne` and after a while main GammaRay window will open.

If, however, nothing happens, try another way to launch it. In `ariadne` repo do:

`stack exec -- ~/path/to/GammaRay/build/bin/gammaray ariadne-qt`

This will start GammaRay alraedy attached to `ariadne-qt` process.

Once main window shows, select `Widgets` tab on the left. This will give you a tree of all widgets, a list
of selected widget's properties on right and live updated view of the window on bottom. Currently selected
widget will get a border. This is useful, for example, to debug various layout problems. Changes to properties
will be applied once you de-focus the input field.

For more information refer to official GammaRay documentation.

## Extending qtah

Sometimes you will find out that `qtah` lacks some method or even a class. In that case you have to add it.

First of all, clone `https://github.com/serokell/qtah/`. This is a mirror of upstream repo
(https://gitlab.com/khumba/qtah) where we keep our local changes before making PR at upstream. Usually
there is a topic branch named `srk-<month>`, and `master` branch which follows upstream master.

When there is a considerable amount of changes in our branch and no major changes are expected in
foreseeable future, push this branch to upstream GitLab (you have to make your own fork there) and make
merge request. Once author merges it, pull `master` from there, push it to our GitHub fork and update
commit hash in `stack.yaml`.

Then you need to instruct `stack` to build your local clone of `qtah`. Assuming that you have `qtah` repo
next to `ariadne`, add these lines to `packages` in `stack.yaml`:

```yaml
- ../qtah/qtah-generator
- ../qtah/qtah-cpp
- ../qtah/qtah
```

Also, comment out `qtah` in `extra-deps` section. Now `stack build` will use local `qtah`.

Here's how to add new class to `qtah`:

1. Create new `hs` file in the right subdir of `qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/`.
   Valid subdirs are `Core`, `Gui` and `Widgets`, this is usually determined by line like `QT += gui` in
   qt documentation. You can copy some existing module like `Core/QObject.hs` or `Widgets/QWidget.hs` and use
   it as reference.
2. Add your module to `qtah-generator/qtah-generator.cabal`.
3. Add your module **twice** to `qtah/qtah.cabal`, once as `Graphics.UI.Qtah.<subdir>`, then as
   `Graphics.UI.Qtah.Generated.<subdir>`.
4. Import your module in top-level module for that subdir (`Gui.hs`, etc.) and add its `aModule` symbol to
   `modules` declaration.

The next step is to actually write your module. Usually you only need to write module name everywhere needed
(`addReqIncludes`, `makeClass`, `makeQtModule`, ...), just follow the example of existing modules. After that
define all methods you are interested in using `mkCtor` / `mkMethod` / `mkConstMethod` / `mkProp`.

Use [the upstream README](https://gitlab.com/khumba/qtah#extending-the-qt-api) for more reference.

When you commit something to topic branch, don't forget to update commit hash in Ariadne `stack.yaml`.
