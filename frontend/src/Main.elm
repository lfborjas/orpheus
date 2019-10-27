module Main exposing (Model, Msg(..), exampleNavbar, fontAwesomeCDN, main, view)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Attribute, Html, a, br, i, img, input, main_, option, p, small, span, strong, text, textarea)
import Html.Attributes exposing (attribute, class, cols, href, placeholder, rel, rows, src, style, type_)


type alias Model =
    {}


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.sandbox
        { init = {}
        , view = view
        , update = \msg -> \model -> model
        }


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
            [ container [] [ editor ] ]
        ]


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


editor : Html Msg
editor =
    textarea [ rows 100, cols 120, class "textarea is-info" ] []
