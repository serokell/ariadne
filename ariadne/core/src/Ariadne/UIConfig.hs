module Ariadne.UIConfig
       ( licenseUrl
       , changelogUrl
       , aboutUrl

       , mnemonicHeaderMessage
       , mnemonicNoLooksMessage
       , mnemonicOnDeviceMessage
       , mnemonicAppMovedMessage
       , mnemonicBeforeMkMessage
       , mnemonicDisplayMessage
       , mnemonicRetypeMessage
       , mnemonicDoneMessage
       , deleteHeaderMkMessage
       , deleteHeaderMessage
       , deleteIntroMkMessage
       , deleteSureMkMessage
       , deleteSureMessage
       , deleteRetypeMkMessage
       , expandingMessageRemovingKeys
       , expandingMessageRemovingWallets
       , sendHeaderMessage
       , sendDefinitiveMessage
       , sendListMessage
       , passwordHeaderMessage
       , passwordUseOneMessage
       , passwordLabelMessage
       , rmUnkownKeysHeaderMessage
       , rmUnknownKeysIntroMessage
       , rmUnknownKeysSureMessage
       , rmUnknownKeysRetypeConfirm
       , rmUnknownKeysRetypeMkMessage
       , rmBrokenWalletsHeaderMessage
       , rmBrokenWalletsIntroMkMessage
       , rmBrokenWalletDelSureMessage
       , rmBrokenWltRetypeMkMessage
       , rmBrokenRetypeConfirm
       , changePasswordHeaderMessage
       , changePasswordInfoMessage
       ) where

import Formatting

licenseUrl :: Text
licenseUrl = "https://serokell.io/ariadne/license"

changelogUrl :: Text
changelogUrl = "https://serokell.io/ariadne/changelog"

aboutUrl :: Text
aboutUrl = "https://github.com/serokell/ariadne"

-- Messages displayed by different UIs
-- Name convention: context + About (+ "Mk" if with input) + "Message"
mnemonicHeaderMessage :: Text
mnemonicHeaderMessage = "Recovery Phrase"

mnemonicNoLooksMessage :: Text
mnemonicNoLooksMessage = "Make sure nobody looks into your screen unless you \
    \want them to have access to your funds."

mnemonicOnDeviceMessage :: Text
mnemonicOnDeviceMessage = "I understand that my money are held securely on \
    \this device only, not on the company servers"

mnemonicAppMovedMessage :: Text
mnemonicAppMovedMessage = "I understand that if this application is moved to\
    \ another device or deleted, my money can be only recovered with the backup\
    \ phrase which was written down in a secure place"

mnemonicBeforeMkMessage :: Int -> Text
mnemonicBeforeMkMessage = sformat $
    "On the following screen, you will see a set of " % int % " random words. \
    \This is your wallet backup phrase. It can be entered in any version of \
    \Ariadne application in order to restore your wallet’s funds and private key."

mnemonicDisplayMessage :: Text
mnemonicDisplayMessage = "Please make sure you have carefully writen down your \
    \recovery phrase somewhere safe. You will need this phrase later for next \
    \use and recover. Phrase is case sensitive."

mnemonicRetypeMessage :: Text
mnemonicRetypeMessage = "Type each word in the correct order to verify your \
    \recovery phrase."

mnemonicDoneMessage :: Text
mnemonicDoneMessage = "You are done!"

deleteHeaderMkMessage :: Format Text (delItem -> Text) -> delItem -> Text
deleteHeaderMkMessage itemTypeFormat itemType =
    sformat ("delete " % itemTypeFormat) itemType

deleteHeaderMessage :: Text
deleteHeaderMessage = "Confirm Deletion"

deleteIntroMkMessage :: Format Text (delItem -> Text) -> Text -> delItem -> Text
deleteIntroMkMessage itemTypeFormat itemName itemType = sformat
    ("Do you really want to delete " % stext % " " % itemTypeFormat % "?")
    itemName itemType

deleteSureMkMessage :: Format Text (delItem -> Text) -> delItem -> Text
deleteSureMkMessage itemTypeFormat itemType = sformat
    (stext % itemTypeFormat % ".")
    deleteSureMessage itemType

deleteSureMessage :: Text
deleteSureMessage = "Make sure you have access to backup before continuing. \
    \Otherwise you will lose all your funds connected to this "

deleteRetypeMkMessage :: Format Text (delItem -> Text) -> delItem -> Text
deleteRetypeMkMessage itemTypeFormat itemType = sformat
    ("Type " % itemTypeFormat % " name to confirm deletion") itemType

rmUnkownKeysHeaderMessage :: Text
rmUnkownKeysHeaderMessage = "Remove unknown keys"

rmUnknownKeysSureMessage :: Text
rmUnknownKeysSureMessage = "Make sure that you don't need keys from keyfile \
    \(It is not a backup, for example). Those keys will be deleted."

rmUnknownKeysIntroMessage :: Text
rmUnknownKeysIntroMessage= "There are keys that don't \
    \correspond to any known wallet.\nDo you want to delete them?"

rmUnknownKeysRetypeMkMessage :: Text -> Text
rmUnknownKeysRetypeMkMessage typeMsg = "Please, write " <> typeMsg <> " if you are sure."

rmUnknownKeysRetypeConfirm :: Text
rmUnknownKeysRetypeConfirm = "\"Yes\""

expandingMessageRemovingKeys :: Text
expandingMessageRemovingKeys = "List of unknown secret keys"

rmBrokenWalletsHeaderMessage :: Text
rmBrokenWalletsHeaderMessage = "Remove wallets with missing secret keys"

rmBrokenWalletsIntroMkMessage :: Text
rmBrokenWalletsIntroMkMessage = "There are wallets without corresponding secret key \
    \in keyfile. Without secret keys you can do almost nothing with such wallets. \
    \Do you want to delete them?"

rmBrokenWalletDelSureMessage :: Text
rmBrokenWalletDelSureMessage = "Make sure that you don't have secret keys or can restore \
    \wallets from mnemonic."

rmBrokenWltRetypeMkMessage :: Text -> Text
rmBrokenWltRetypeMkMessage typeMsg = "Please, write " <> typeMsg <> " if you are sure."

rmBrokenRetypeConfirm :: Text
rmBrokenRetypeConfirm = "\"Yes\""

expandingMessageRemovingWallets :: Text
expandingMessageRemovingWallets = "List of wallets with missing secret keys"

sendHeaderMessage :: Text
sendHeaderMessage = "Confirm Transaction"

sendDefinitiveMessage :: Text
sendDefinitiveMessage = "I understand that continuing with this operation will \
    \make it definitive and irreversible."

sendListMessage :: Text
sendListMessage = "This is the list of this operation's output transitions. \
    \Please review them carefully."

passwordHeaderMessage :: Text
passwordHeaderMessage = "Insert Password"

passwordUseOneMessage :: Text
passwordUseOneMessage = "Activate to use a password."

passwordLabelMessage :: Text
passwordLabelMessage = "Password"

changePasswordHeaderMessage :: Text
changePasswordHeaderMessage = "Change password"

changePasswordInfoMessage :: Text
changePasswordInfoMessage = "Make sure to remember your password. \
    \Losing your password requires resetting wallet"
