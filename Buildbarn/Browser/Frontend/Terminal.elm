module Buildbarn.Browser.Frontend.Terminal exposing
    ( Attributes
    , Color(..)
    , FormattedTextFragment
    , FormattedTextFragments
    , InputSequence(..)
    , defaultAttributes
    , formattedTextFragments
    , inputSequence
    )

import Html exposing (Html)
import Parser exposing (Parser)


textFragment : Parser String
textFragment =
    (\c -> c /= '\u{001B}')
        |> Parser.chompWhile
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "text fragment must be non-empty"

                else
                    Parser.succeed s
            )


selectGraphicRendition : Parser (List Int)
selectGraphicRendition =
    Parser.sequence
        { start = "\u{001B}["
        , separator = ";"
        , end = "m"
        , spaces = Parser.succeed ()
        , item = Parser.int
        , trailing = Parser.Forbidden
        }


type InputSequence
    = TextFragment String
    | SelectGraphicRendition (List Int)


inputSequence : Parser InputSequence
inputSequence =
    Parser.oneOf
        [ Parser.map TextFragment textFragment
        , Parser.map SelectGraphicRendition selectGraphicRendition
        ]


type Color
    = Default
    | Black
    | Red
    | Green
    | Brown
    | Blue
    | Magenta
    | Cyan
    | White


type alias Attributes =
    { bold : Bool
    , underline : Bool
    , blink : Bool
    , reverse : Bool
    , foreground : Color
    , background : Color
    }


defaultAttributes : Attributes
defaultAttributes =
    { bold = False
    , underline = False
    , blink = False
    , reverse = False
    , foreground = Default
    , background = Default
    }


applyAttribute : Int -> Attributes -> Attributes
applyAttribute code attributes =
    case code of
        0 ->
            defaultAttributes

        1 ->
            { attributes | bold = True }

        4 ->
            { attributes | underline = True }

        5 ->
            { attributes | blink = True }

        7 ->
            { attributes | reverse = True }

        22 ->
            { attributes | bold = False }

        24 ->
            { attributes | underline = False }

        25 ->
            { attributes | blink = False }

        27 ->
            { attributes | reverse = False }

        30 ->
            { attributes | foreground = Black }

        31 ->
            { attributes | foreground = Red }

        32 ->
            { attributes | foreground = Green }

        33 ->
            { attributes | foreground = Brown }

        34 ->
            { attributes | foreground = Blue }

        35 ->
            { attributes | foreground = Magenta }

        36 ->
            { attributes | foreground = Cyan }

        37 ->
            { attributes | foreground = White }

        39 ->
            { attributes | foreground = Default }

        40 ->
            { attributes | background = Black }

        41 ->
            { attributes | background = Red }

        42 ->
            { attributes | background = Green }

        43 ->
            { attributes | background = Brown }

        44 ->
            { attributes | background = Blue }

        45 ->
            { attributes | background = Magenta }

        46 ->
            { attributes | background = Cyan }

        47 ->
            { attributes | background = White }

        49 ->
            { attributes | background = Default }

        _ ->
            -- Unknown attribute code (e.g., 256 colors). Skip these for now.
            attributes


type alias FormattedTextFragment =
    ( Attributes, String )


type alias FormattedTextFragments =
    { textFragments : List FormattedTextFragment
    , finalAttributes : Attributes
    }


applyInputSequence : FormattedTextFragments -> InputSequence -> FormattedTextFragments
applyInputSequence state sequence =
    case sequence of
        TextFragment text ->
            -- TODO: Recombine fragments with identical attributes?
            { state | textFragments = state.textFragments ++ [ ( state.finalAttributes, text ) ] }

        SelectGraphicRendition codes ->
            if List.isEmpty codes then
                { state | finalAttributes = defaultAttributes }

            else
                { state | finalAttributes = List.foldl applyAttribute state.finalAttributes codes }


formattedTextFragments : Attributes -> Parser FormattedTextFragments
formattedTextFragments initialAttributes =
    Parser.loop
        { textFragments = [], finalAttributes = initialAttributes }
    <|
        \state ->
            Parser.oneOf
                [ inputSequence
                    |> Parser.map (\s -> Parser.Loop (applyInputSequence state s))
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done state)
                ]
