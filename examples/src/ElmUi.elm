module ElmUi exposing (main)

import Browser
import Browser.Dom
import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    { active : String
    , activation : Tabs.Activation
    }


type Msg
    = NoOp
    | UserChangedActivation Tabs.Activation
    | UserChangedTab String (Task Browser.Dom.Error ())


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = "Nils Frahm"
      , activation = Tabs.Automatic
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        UserChangedActivation activation ->
            ( { model | activation = activation }, Cmd.none )

        UserChangedTab active task ->
            ( { model | active = active }
            , Task.attempt (\_ -> NoOp) task
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Tabs.viewCustom views
            { tabs = List.map toTab allTabs
            , active = model.active
            , label = "Entertainment"
            , orientation = Tabs.Horizontal
            , activation = model.activation
            , onChange = UserChangedTab
            }
        )


toTab : ( String, String ) -> Tabs.Tab (Element Msg) String
toTab ( title, content ) =
    { tag = title
    , id = title
    , label = Element.text title
    , panel =
        Element.paragraph []
            [ Element.text content ]
    , focusable = True
    }


views : Tabs.Views (Element msg) msg
views =
    { tabs =
        \attrs tabs panels ->
            Element.column
                [ attribute "role" attrs.role
                , attribute "aria-label" attrs.ariaLabel
                ]
                [ Element.row [] tabs
                , Element.column [] panels
                ]
    , tab =
        \attrs label ->
            let
                withTabindex elAttrs =
                    case attrs.tabindex of
                        Nothing ->
                            elAttrs

                        Just tabindex ->
                            attribute "tabindex" (String.fromInt tabindex)
                                :: elAttrs
            in
            Input.button
                ([ attribute "role" attrs.role
                 , attribute "aria-selected"
                    (if attrs.ariaSelected then
                        "true"

                     else
                        "false"
                    )
                 , attribute "aria-controls" attrs.ariaControls
                 , attribute "id" attrs.id
                 , preventDefaultOn "keydown" attrs.preventDefaultOnKeydown
                 , Element.padding 8
                 , Border.width 2
                 , Element.focused
                    [ Border.color (Element.rgb 0 0 1) ]
                 ]
                    |> withTabindex
                )
                { onPress = Just attrs.onClick
                , label = label
                }
    , panel =
        \attrs panel ->
            let
                withHidden elAttrs =
                    if attrs.hidden then
                        style "display" "none" :: elAttrs

                    else
                        elAttrs

                withTabindex elAttrs =
                    case attrs.tabindex of
                        Nothing ->
                            elAttrs

                        Just tabindex ->
                            attribute "tabindex" (String.fromInt tabindex)
                                :: elAttrs
            in
            Element.el
                ([ attribute "role" attrs.role
                 , attribute "id" attrs.id
                 , attribute "aria-labelledby" attrs.ariaLabelledby
                 , Element.padding 8
                 , Border.width 2
                 , Element.focused
                    [ Border.color (Element.rgb 0 0 1) ]
                 ]
                    |> withHidden
                    |> withTabindex
                )
                panel
    }


attribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)


preventDefaultOn event decoder =
    Element.htmlAttribute (Html.Events.preventDefaultOn event decoder)


style property value =
    Element.htmlAttribute (Html.Attributes.style property value)


{-| Taken from <https://w3c.github.io/aria-practices/examples/tabs/tabs-2/tabs.html>
-}
allTabs : List ( String, String )
allTabs =
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
