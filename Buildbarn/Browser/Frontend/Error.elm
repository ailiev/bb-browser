module Buildbarn.Browser.Frontend.Error exposing (Error(..))

import Http


type Error
    = Http Http.Error
    | Loading
