module Buildbarn.Browser.Frontend.Error exposing (Error(..))

import Http
import Parser


type Error
    = Http Http.Error
    | Loading
    | Parser (List Parser.DeadEnd)
