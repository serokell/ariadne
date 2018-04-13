module Ariadne.UI.Vty.Keyboard where

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

  | KeyEditLeft
  | KeyEditRight
  | KeyEditLeftWord
  | KeyEditRightWord
  | KeyEditHome
  | KeyEditEnd
  | KeyEditDelLeft
  | KeyEditDelLeftWord
  | KeyEditDelRight
  | KeyEditDelRightWord

  | KeyEditAutocomplete
  | KeyEditPrev
  | KeyEditNext
  | KeyEditNewLine
  | KeyEditSend
  | KeyEditCancel
  | KeyEditExit

  | KeyChar Char
  | KeyUnknown
  deriving (Eq)

vtyToKey :: Bool -> Bool -> Event -> KeyboardEvent
vtyToKey navMode editMode
  | navMode = \case
    EvKey (KChar 'c')  [MCtrl] -> KeyExit
    EvKey KEsc         []      -> KeyNavigation

    EvKey (KChar '\t') []      -> KeyFocusNext
    EvKey (KChar '\t') [MCtrl] -> KeyFocusNext
    EvKey KBackTab     []      -> KeyFocusPrev
    EvKey KBackTab     [MCtrl] -> KeyFocusPrev

    EvKey (KChar c)    []      -> KeyChar c
    _                          -> KeyUnknown
  | editMode = \case
    EvKey (KChar 'd')  [MCtrl] -> KeyExit
    EvKey KEsc         []      -> KeyNavigation

    EvKey (KChar '\t') [MCtrl] -> KeyFocusNext
    EvKey KBackTab     [MCtrl] -> KeyFocusPrev

    EvKey KUp          [MCtrl] -> KeyUp
    EvKey KDown        [MCtrl] -> KeyDown
    EvKey KPageUp      []      -> KeyPageUp
    EvKey KPageUp      [MCtrl] -> KeyPageUp
    EvKey KPageDown    []      -> KeyPageDown
    EvKey KPageDown    [MCtrl] -> KeyPageDown
    EvKey KHome        [MCtrl] -> KeyHome
    EvKey KEnd         [MCtrl] -> KeyEnd

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
    EvKey KDel         []      -> KeyEditDelRight
    EvKey KDel         [MCtrl] -> KeyEditDelRightWord

    EvKey (KChar '\t') []      -> KeyEditAutocomplete
    EvKey KUp          []      -> KeyEditPrev
    EvKey KDown        []      -> KeyEditNext
    EvKey KEnter       [MCtrl] -> KeyEditNewLine
    EvKey KEnter       []      -> KeyEditSend
    EvKey (KChar 'c')  [MCtrl] -> KeyEditCancel

    EvKey (KChar c)    []      -> KeyChar c
    _                          -> KeyUnknown

  | otherwise = \case
    EvKey (KChar 'c')  [MCtrl] -> KeyExit
    EvKey KEsc         []      -> KeyNavigation

    EvKey (KChar '\t') []      -> KeyFocusNext
    EvKey (KChar '\t') [MCtrl] -> KeyFocusNext
    EvKey KBackTab     []      -> KeyFocusPrev
    EvKey KBackTab     [MCtrl] -> KeyFocusPrev
    EvKey KEnter       []      -> KeyEnter

    EvKey KLeft        []      -> KeyLeft
    EvKey KLeft        [MCtrl] -> KeyLeft
    EvKey (KChar 'h')  []      -> KeyLeft
    EvKey KRight       []      -> KeyRight
    EvKey KRight       [MCtrl] -> KeyRight
    EvKey (KChar 'l')  []      -> KeyRight
    EvKey KUp          []      -> KeyUp
    EvKey KUp          [MCtrl] -> KeyUp
    EvKey (KChar 'k')  []      -> KeyUp
    EvKey KDown        []      -> KeyDown
    EvKey KDown        [MCtrl] -> KeyDown
    EvKey (KChar 'j')  []      -> KeyDown
    EvKey KPageUp      []      -> KeyPageUp
    EvKey KPageUp      [MCtrl] -> KeyPageUp
    EvKey KPageDown    []      -> KeyPageDown
    EvKey KPageDown    [MCtrl] -> KeyPageDown
    EvKey KHome        []      -> KeyHome
    EvKey KHome        [MCtrl] -> KeyHome
    EvKey KEnd         []      -> KeyEnd
    EvKey KEnd         [MCtrl] -> KeyEnd

    EvKey (KChar c)    []      -> KeyChar c
    _                          -> KeyUnknown
