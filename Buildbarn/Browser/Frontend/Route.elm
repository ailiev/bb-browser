module Buildbarn.Browser.Frontend.Route exposing (Digest, Route(..), fromUrl)

import Url
import Url.Parser as Parser exposing ((</>))


type alias Digest =
    { instance : String
    , hash : String
    , sizeBytes : Int
    }


digestParser : Parser.Parser (Digest -> a) a
digestParser =
    Parser.map Digest (Parser.string </> Parser.string </> Parser.int)


type Route
    = Action Digest
    | Command Digest
    | Directory Digest
    | Tree Digest (List String)
    | UncachedActionResult Digest
    | Welcome


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Action (Parser.s "action" </> digestParser)
        , Parser.map Command (Parser.s "command" </> digestParser)
        , Parser.map Directory (Parser.s "directory" </> digestParser)
        , Parser.map Tree (Parser.s "tree" </> digestParser </> Parser.remainder)
        , Parser.map UncachedActionResult (Parser.s "uncached_action_result" </> digestParser)
        , Parser.map Welcome Parser.top
        ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
