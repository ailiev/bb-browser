module Buildbarn.Browser.Frontend.Api exposing
    ( CallResult
    , getChildMessage
    , getMessage
    )

import Build.Bazel.Remote.Execution.V2.Remote_execution as REv2
import Buildbarn.Browser.Frontend.Digest as Digest exposing (Digest)
import Http
import Json.Decode as JD
import Url.Builder


type alias CallResult message =
    Result Http.Error message


getMessage : String -> (Digest -> CallResult a -> msg) -> JD.Decoder a -> Digest -> Cmd msg
getMessage endpoint toMsg decoder digest =
    Http.get
        { url =
            Url.Builder.relative
                [ "api", "get_" ++ endpoint ]
                [ Url.Builder.string "instance" digest.instance
                , Url.Builder.string "hash" digest.hash
                , Url.Builder.int "size_bytes" digest.sizeBytes
                ]
        , expect = Http.expectJson (toMsg digest) decoder
        }


getChildMessage : String -> (Digest -> CallResult a -> msg) -> JD.Decoder a -> (b -> Maybe REv2.DigestMessage) -> Digest -> CallResult b -> Cmd msg
getChildMessage endpoint toMsg decoder getChildDigest parentDigest parentResult =
    case parentResult of
        Ok parent ->
            case getChildDigest parent of
                Just (REv2.DigestMessage childDigest) ->
                    getMessage endpoint
                        toMsg
                        decoder
                        (Digest.getDerived parentDigest childDigest)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none
