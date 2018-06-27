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
  | KeyCtrlUp
  | KeyCtrlDown
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
  | KeyEditLeftWord
  | KeyEditRightWord
  | KeyEditHome
  | KeyEditEnd
  | KeyEditUp
  | KeyEditDown
  | KeyEditDelLeft
  | KeyEditDelLeftWord
  | KeyEditDelLeftAll
  | KeyEditDelRight
  | KeyEditDelRightWord

  | KeyEditPrev
  | KeyEditNext
  | KeyEditSend
  | KeyEditCancel

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
    EvKey KUp          [MCtrl]  -> KeyCtrlUp
    EvKey KDown        [MCtrl]  -> KeyCtrlDown
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
    EvKey KLeft        [MCtrl]  -> KeyEditLeftWord
    EvKey KRight       [MCtrl]  -> KeyEditRightWord
    EvKey KHome        []       -> KeyEditHome
    EvKey KEnd         []       -> KeyEditEnd
    EvKey KUp          []       -> KeyEditUp
    EvKey KDown        []       -> KeyEditDown

    EvKey KBS          []       -> KeyEditDelLeft
    EvKey (KChar 'h')  [MCtrl]  -> KeyEditDelLeft
    EvKey KBS          [MCtrl]  -> KeyEditDelLeftWord
    EvKey (KChar 'w')  [MCtrl]  -> KeyEditDelLeftWord
    EvKey (KChar 'u')  [MCtrl]  -> KeyEditDelLeftAll
    EvKey KDel         []       -> KeyEditDelRight
    EvKey KDel         [MCtrl]  -> KeyEditDelRightWord

    EvKey (KChar 'p')  [MCtrl]  -> KeyEditPrev
    EvKey (KChar 'n')  [MCtrl]  -> KeyEditNext
    EvKey KEnter       []       -> KeyEditSend
    EvKey (KChar 'c')  [MCtrl]  -> KeyEditCancel

    EvKey (KChar c)    []       -> KeyEditChar c
    _                           -> KeyEditUnknown
