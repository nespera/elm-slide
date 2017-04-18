module Msg exposing (..)
import Keyboard exposing (KeyCode)

type Msg
  = Choose String | Pressed KeyCode | Reset