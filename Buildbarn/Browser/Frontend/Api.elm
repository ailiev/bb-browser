module Buildbarn.Browser.Frontend.Api exposing (CallResult, getMessage)

import Buildbarn.Browser.Frontend.Route as Route
import Http
import Json.Decode as JD
import Url.Builder


type alias CallResult message =
    Result Http.Error message


getMessage : String -> (CallResult a -> msg) -> JD.Decoder a -> Route.Digest -> Cmd msg
getMessage endpoint toMsg decoder digest =
    Http.get
        { url =
            Url.Builder.relative
                [ "api", "get_" ++ endpoint ]
                [ Url.Builder.string "instance" digest.instance
                , Url.Builder.string "hash" digest.hash
                , Url.Builder.int "size_bytes" digest.sizeBytes
                ]
        , expect = Http.expectJson toMsg decoder
        }
