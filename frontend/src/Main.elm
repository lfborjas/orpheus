module Main exposing (Model, Msg(..), countLines, countStanzas, countWords, exampleNavbar, fontAwesomeCDN, main, view)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Attribute, Html, a, br, div, i, img, input, main_, option, p, small, span, strong, text, textarea)
import Html.Attributes exposing (attribute, class, cols, href, placeholder, rel, rows, src, style, type_)
import Html.Events exposing (onInput)
import Maybe
import Regex



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


fontAwesomeCDN =
    Html.node "link"
        [ rel "stylesheet"
        , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
        ]
        []


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , fontAwesomeCDN
        , exampleNavbar
        , section NotSpaced
            []
            [ container []
                [ columns columnsModifiers
                    []
                    [ column (myColumnModifiers Auto (Just Width2)) [] []
                    , column (myColumnModifiers Auto (Just Auto)) [] [ editor model ]
                    ]
                ]
            ]
        ]



-- from: https://github.com/surprisetalk/elm-bulma/blob/4b46323c581e03b8d80167102459d54782336817/src/Example.elm#L98


myColumnModifiers : Width -> Maybe Width -> ColumnModifiers
myColumnModifiers offset width =
    let
        widths : Devices (Maybe Width)
        widths =
            columnModifiers.widths
    in
    { columnModifiers
        | offset =
            offset
        , widths =
            { widths
                | tablet = width
                , desktop = width
                , widescreen = width
                , fullHD = width
            }
    }


myBurger : Bool -> NavbarBurger Msg
myBurger isMenuOpen =
    navbarBurger isMenuOpen
        []
        [ span [] [] ]


exampleNavbar : Html Msg
exampleNavbar =
    navbar { navbarModifiers | color = Dark }
        []
        [ navbarBrand []
            (myBurger False)
            [ navbarItem False
                []
                [ title H4 [ class "has-text-light" ] [ text "Orpheus" ] ]
            ]
        , navbarMenu False
            []
            [ navbarStart []
                [ navbarItemLink False [] [ text "Home" ] ]
            , navbarEnd [] []
            ]
        ]


stats : Model -> Html Msg
stats model =
    div []
        [ centeredLevel []
            [ statsTag "Words" <| countWords model.content
            , statsTag "Lines" <| countLines model.content
            , statsTag "Stanzas" <| countStanzas model.content
            ]
        ]


statsTag : String -> String -> LevelItem Msg
statsTag lbl txt =
    easyLevelItemWithHeading [] lbl txt


editor : Model -> Html Msg
editor model =
    div []
        [ stats model
        , textarea [ rows 100, cols 120, class "textarea is-info", onInput Change ] []
        ]



-- PURE FNS


countWords : String -> String
countWords s =
    String.words s
        |> List.length
        |> String.fromInt


countLines : String -> String
countLines s =
    String.lines s
        |> List.length
        |> String.fromInt



-- with some help from:
-- http://jsfiddle.net/ahRHC/2/
-- (https://stackoverflow.com/questions/22272590/count-number-of-paragraphs-excluding-spaces)


stanzaPattern : Regex.Regex
stanzaPattern =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[\\r\\n]{2,}(?!\\s*$)"


countStanzas : String -> String
countStanzas s =
    Regex.split stanzaPattern s
        |> List.length
        |> String.fromInt
