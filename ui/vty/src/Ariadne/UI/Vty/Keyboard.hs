module Ariadne.UI.Vty.Keyboard
     ( KeyboardEvent(..)
     , KeyboardEditEvent(..)
     , vtyToKey
     , vtyToEditKey
     ) where

import Universum

import Graphics.Vty

data KeyboardEvent
  = KeyUnknown

  | KeyQuit
  | KeyNavigation
  | KeyFocusNext
  | KeyFocusPrev
  | KeyEnter

  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd

  | KeyChar Char
  deriving (Eq)

data KeyboardEditEvent
  = KeyEditUnknown
  | KeyEditQuit

  | KeyEditLeft
  | KeyEditRight
  | KeyEditUp
  | KeyEditDown
  | KeyEditDelLeft
  | KeyEditDelLeftWord
  | KeyEditDelRight

  | KeyEditPrev
  | KeyEditNext
  | KeyEditSend

  | KeyEditChar Char
  deriving (Eq)

vtyToKey :: Event -> KeyboardEvent
vtyToKey = \case
    EvKey (KChar 'c')  [MCtrl]  -> KeyQuit

    EvKey KEsc         []       -> KeyNavigation
    EvKey (KChar '\t') []       -> KeyFocusNext
    EvKey KBackTab     []       -> KeyFocusPrev
    EvKey KEnter       []       -> KeyEnter

    EvKey KLeft        _        -> KeyLeft
    EvKey KRight       _        -> KeyRight
    EvKey KUp          _        -> KeyUp
    EvKey KDown        _        -> KeyDown
    EvKey KPageUp      _        -> KeyPageUp
    EvKey KPageDown    _        -> KeyPageDown
    EvKey KHome        _        -> KeyHome
    EvKey KEnd         _        -> KeyEnd

    EvKey (KChar c)    []       -> KeyChar c
    _                           -> KeyUnknown

vtyToEditKey :: Event -> KeyboardEditEvent
vtyToEditKey = \case
    EvKey (KChar 'd')  [MCtrl]  -> KeyEditQuit

    EvKey KLeft        []       -> KeyEditLeft
    EvKey KRight       []       -> KeyEditRight
    EvKey KUp          []       -> KeyEditUp
    EvKey KDown        []       -> KeyEditDown

    EvKey KBS          []       -> KeyEditDelLeft
    EvKey (KChar 'h')  [MCtrl]  -> KeyEditDelLeft
    EvKey KBS          [MCtrl]  -> KeyEditDelLeftWord
    EvKey (KChar 'w')  [MCtrl]  -> KeyEditDelLeftWord
    EvKey KDel         []       -> KeyEditDelRight

    EvKey (KChar 'p')  [MCtrl]  -> KeyEditPrev
    EvKey (KChar 'n')  [MCtrl]  -> KeyEditNext
    EvKey KEnter       []       -> KeyEditSend

    EvKey (KChar c)    []       -> KeyEditChar c
    _                           -> KeyEditUnknown