module CustomHtml exposing (main)

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
    Html.div
        []
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
        , Tabs.view views
            { tabs = List.map (toTab "automatic") tabs
            , active = model.activeAutomatic
            , label = Tabs.label "Entertainment"
            , orientation = Tabs.Horizontal
            , activation = Tabs.Automatic
            , onChange = UserChangedTabAutomatic
            }
        , Html.h2 []
            [ Html.text "Manual activation" ]
        , Tabs.view views
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



---- CSS


views : Tabs.Views (Html msg) msg
views =
    Tabs.html
        { container = [ Html.Attributes.class "tabs" ]
        , tabList = []
        , tab = always []
        , panel = always []
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


{-| Taken from <https://w3c.github.io/aria-practices/examples/tabs/css/tabs.css>
-}
css : String
css =
    """
body {
  margin: auto;
  max-width: 600px;
}

[role~="tablist"] {
  margin: 0 0 -0.1em;
  overflow: visible;
}

[role~="tab"] {
  position: relative;
  margin: 0;
  padding: 0.3em 0.5em 0.4em;
  border: 1px solid hsl(219, 1%, 72%);
  border-radius: 0.2em 0.2em 0 0;
  box-shadow: 0 0 0.2em hsl(219, 1%, 72%);
  overflow: visible;
  font-family: inherit;
  font-size: inherit;
  background: hsl(220, 20%, 94%);
}

[role~="tab"]:hover::before,
[role~="tab"]:focus::before,
[role~="tab"][aria-selected="true"]::before {
  position: absolute;
  bottom: 100%;
  right: -1px;
  left: -1px;
  border-radius: 0.2em 0.2em 0 0;
  border-top: 3px solid hsl(20, 96%, 48%);
  content: "";
}

[role~="tab"][aria-selected="true"] {
  border-radius: 0;
  background: hsl(220, 43%, 99%);
  outline: 0;
}

[role~="tab"][aria-selected="true"]:not(:focus):not(:hover)::before {
  border-top: 5px solid hsl(218, 96%, 48%);
}

[role~="tab"][aria-selected="true"]::after {
  position: absolute;
  z-index: 3;
  bottom: -1px;
  right: 0;
  left: 0;
  height: 0.3em;
  background: hsl(220, 43%, 99%);
  box-shadow: none;
  content: "";
}

[role~="tab"]:hover,
[role~="tab"]:focus,
[role~="tab"]:active {
  outline: 0;
  border-radius: 0;
  color: inherit;
}

[role~="tab"]:hover::before,
[role~="tab"]:focus::before {
  border-color: hsl(20, 96%, 48%);
}

[role~="tabpanel"] {
  position: relative;
  z-index: 2;
  padding: 0.5em 0.5em 0.7em;
  border: 1px solid hsl(219, 1%, 72%);
  border-radius: 0 0.2em 0.2em 0.2em;
  box-shadow: 0 0 0.2em hsl(219, 1%, 72%);
  background: hsl(220, 43%, 99%);
}

[role~="tabpanel"]:focus {
  border-color: hsl(20, 96%, 48%);
  box-shadow: 0 0 0.2em hsl(20, 96%, 48%);
  outline: 0;
}

[role~="tabpanel"]:focus::after {
  position: absolute;
  bottom: 0;
  right: -1px;
  left: -1px;
  border-bottom: 3px solid hsl(20, 96%, 48%);
  border-radius: 0 0 0.2em 0.2em;
  content: "";
}

[role~="tabpanel"] p {
  margin: 0;
}

[role~="tabpanel"] * + p {
  margin-top: 1em;
}
    """
