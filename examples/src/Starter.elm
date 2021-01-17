module Starter exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Tabs
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { activeAutomatic : String
    , activeManual : String
    }


type Msg
    = NoOp
    | UserChangedTabAutomatic String (Task Browser.Dom.Error ())
    | UserChangedTabManual String (Task Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeAutomatic = "Nils Frahm"
      , activeManual = "Nils Frahm"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UserChangedTabAutomatic active task ->
            ( { model | activeAutomatic = active }
            , Task.attempt (\_ -> NoOp) task
            )

        UserChangedTabManual active task ->
            ( { model | activeManual = active }
            , Task.attempt (\_ -> NoOp) task
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.h1 []
            [ Html.text "Tabs" ]
        , Html.p []
            [ Html.text """
                This package offers an implementation of the Tabs widget as specified in the
                WAI-ARIA Authoring Practices 1.1:
                """
            ]
        , Html.blockquote []
            [ Html.text
                """
                Tabs are a set of layered sections of
                content, known as tab panels, that display one panel of content at a time.
                Each tab panel has an associated tab element, that when activated, displays
                the panel. The list of tab elements is arranged along one edge of the
                currently displayed panel, most commonly the top edge.
                """
            ]
        , Html.h2 []
            [ Html.text "Automatic activation" ]
        , Tabs.viewStarter
            { tabs = List.map (toTab "automatic") tabs
            , active = model.activeAutomatic
            , label = Tabs.label "Entertainment"
            , orientation = Tabs.Horizontal
            , activation = Tabs.Automatic
            , onChange = UserChangedTabAutomatic
            }
        , Html.h2 []
            [ Html.text "Manual activation" ]
        , Tabs.viewStarter
            { tabs = List.map (toTab "manual") tabs
            , active = model.activeManual
            , label = Tabs.label "Entertainment"
            , orientation = Tabs.Horizontal
            , activation = Tabs.Manual
            , onChange = UserChangedTabManual
            }
        ]


toTab : String -> ( String, String ) -> Tabs.Tab (Html Msg) String
toTab id ( title, content ) =
    { tag = title
    , id = title ++ "-" ++ id
    , label = Html.text title
    , panel = Html.text content
    , focusable = True
    }


{-| Taken from <https://w3c.github.io/aria-practices/examples/tabs/tabs-2/tabs.html>
-}
tabs : List ( String, String )
tabs =
    [ ( "Nils Frahm"
      , "Nils Frahm is a German musician, composer and record producer based in Berlin. He is known for combining classical and electronic music and for an unconventional approach to the piano in which he mixes a grand piano, upright piano, Roland Juno-60, Rhodes piano, drum machine, and Moog Taurus."
      )
    , ( "Agnes Obel"
      , "Agnes Caroline Thaarup Obel is a Danish singer/songwriter. Her first album, Philharmonics, was released by PIAS Recordings on 4 October 2010 in Europe. Philharmonics was certified gold in June 2011 by the Belgian Entertainment Association (BEA) for sales of 10,000 Copies."
      )
    , ( "Joke"
      , "Fear of complicated buildings:\n\nA complex complex complex."
      )
    ]


css : String
css =
    """
body {
  margin: auto;
  max-width: 600px;
}
"""
