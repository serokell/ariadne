module Ariadne.UI.Vty.UI
       ( BrickName(..)
       , handleFormFieldEvent
       ) where

import Universum

import Brick
import Brick.Forms

-- | Brick-specific ID for viewports, rendering cache, focus etc
data BrickName
  = BrickMenu
  | BrickTree
  | BrickPane
  | BrickReplOutput
  | BrickReplInput
  | BrickHelp
  | BrickLogs

  | BrickNewWalletName
  | BrickNewWalletPass
  | BrickNewWalletCreateButton
  | BrickNewWalletRestoreName
  | BrickNewWalletRestoreMnemonic
  | BrickNewWalletRestorePass
  | BrickNewWalletRestoreFull
  | BrickNewWalletRestoreButton

  | BrickNone  -- For empty focus selection
  deriving (Eq, Ord, Show)

handleFormFieldEvent
  :: (Eq n)
  => n
  -> BrickEvent n e
  -> Lens' s (FormFieldState s e n)
  -> FormFieldState s e n
  -> s
  -> EventM n s
handleFormFieldEvent n ev fieldLens (FormFieldState fieldState dataLens subFields frender fconcat) st =
  findField subFields
  where
    findField [] = return st
    findField (field:rest) =
      case field of
        FormField n' validate _ _ handleFunc | n == n' -> do
          nextSt <- handleFunc ev fieldState
          return $ st & fieldLens .~ FormFieldState nextSt dataLens subFields frender fconcat &
            case validate nextSt of
              Nothing -> identity
              Just newSt -> dataLens .~ newSt
        _ -> findField rest
