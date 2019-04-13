module Buildbarn.Browser.Frontend.Api exposing
    ( CallResult
    , Digest
    , getDerivedDigest
    , getMessage
    )

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Http
import Json.Decode as JD
import Url.Builder


type alias Digest =
    { instance : String
    , hash : String
    , sizeBytes : Int
    }


type alias CallResult message =
    Result Http.Error message


getDerivedDigest : Digest -> REv2.Digest -> Digest
getDerivedDigest parent child =
    { instance = parent.instance
    , hash = child.hash
    , sizeBytes = child.sizeBytes
    }


getMessage : String -> (CallResult a -> msg) -> JD.Decoder a -> Digest -> Cmd msg
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
