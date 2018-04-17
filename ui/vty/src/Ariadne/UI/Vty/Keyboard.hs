module Ariadne.UI.Vty.Keyboard
     ( KeyboardEvent(..)
     , KeyboardEditEvent(..)
     , vtyToKey
     , vtyToEditKey
     ) where

import Universum

import Graphics.Vty

data KeyboardEvent
  = KeyExit
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
  | KeyUnknown
  deriving (Eq)

data KeyboardEditEvent
  = KeyEditLeft
  | KeyEditRight
  | KeyEditLeftWord
  | KeyEditRightWord
  | KeyEditHome
  | KeyEditEnd
  | KeyEditDelLeft
  | KeyEditDelLeftWord
  | KeyEditDelLeftAll
  | KeyEditDelRight
  | KeyEditDelRightWord

  | KeyEditAutocomplete
  | KeyEditPrev
  | KeyEditNext
  | KeyEditNewLine
  | KeyEditSend
  | KeyEditCancel

  | KeyEditChar Char
  | KeyEditUnknown
  deriving (Eq)

vtyToKey :: Event -> KeyboardEvent
vtyToKey = \case
    EvKey (KChar 'c')  [MCtrl] -> KeyExit
    EvKey (KChar 'd')  [MCtrl] -> KeyExit
    EvKey (KChar 'q')  [MCtrl] -> KeyExit
    EvKey KEsc         []      -> KeyNavigation

    EvKey (KChar '\t') []      -> KeyFocusNext
    EvKey (KChar '\t') [MCtrl] -> KeyFocusNext
    EvKey KBackTab     []      -> KeyFocusPrev
    EvKey KBackTab     [MCtrl] -> KeyFocusPrev
    EvKey KEnter       []      -> KeyEnter

    EvKey KLeft        _       -> KeyLeft
    EvKey KRight       _       -> KeyRight
    EvKey KUp          _       -> KeyUp
    EvKey KDown        _       -> KeyDown
    EvKey KPageUp      _       -> KeyPageUp
    EvKey KPageDown    _       -> KeyPageDown
    EvKey KHome        _       -> KeyHome
    EvKey KEnd         _       -> KeyEnd

    EvKey (KChar c)    []      -> KeyChar c
    _                          -> KeyUnknown

vtyToEditKey :: Event -> KeyboardEditEvent
vtyToEditKey = \case
    EvKey KLeft        []      -> KeyEditLeft
    EvKey KRight       []      -> KeyEditRight
    EvKey KLeft        [MCtrl] -> KeyEditLeftWord
    EvKey KRight       [MCtrl] -> KeyEditRightWord
    EvKey KHome        []      -> KeyEditHome
    EvKey KEnd         []      -> KeyEditEnd

    EvKey KBS          []      -> KeyEditDelLeft
    EvKey (KChar 'h')  [MCtrl] -> KeyEditDelLeft
    EvKey KBS          [MCtrl] -> KeyEditDelLeftWord
    EvKey (KChar 'w')  [MCtrl] -> KeyEditDelLeftWord
    EvKey (KChar 'u')  [MCtrl] -> KeyEditDelLeftAll
    EvKey KDel         []      -> KeyEditDelRight
    EvKey KDel         [MCtrl] -> KeyEditDelRightWord

    EvKey (KChar '\t') []      -> KeyEditAutocomplete
    EvKey KUp          []      -> KeyEditPrev
    EvKey KDown        []      -> KeyEditNext
    EvKey KEnter       []      -> KeyEditSend
    EvKey (KChar 'c')  [MCtrl] -> KeyEditCancel

    EvKey (KChar c)    []      -> KeyEditChar c
    _                          -> KeyEditUnknown
