module ElmUi exposing (main)

import Accessibility.Tabs as Tabs
import BeautifulExample
import Browser
import Browser.Dom
import Color
import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task exposing (Task)


main : Program () Model Msg
main =
    BeautifulExample.element
        { title = "Tabs"
        , details = Just """
            This package offers an implementation of the Tabs widget as specified in the
            WAI-ARIA Authoring Practices 1.1: "Tabs are a set of layered sections of
            content, known as tab panels, that display one panel of content at a time.
            Each tab panel has an associated tab element, that when activated, displays
            the panel. The list of tab elements is arranged along one edge of the
            currently displayed panel, most commonly the top edge."
        """
        , color = Just Color.black
        , maxWidth = 800
        , githubUrl = Just "https://github.com/kirchner/elm-wai-aria-tabs"
        , documentationUrl = Just "https://package.elm-lang.org/packages/kirchner/elm-wai-aria-tabs/latest/"
        }
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
    Element.layout
        [ Element.padding 64 ]
        (Element.column
            [ Element.width (Element.maximum 512 Element.fill)
            , Element.spacing 48
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.spacing 32
                ]
                [ Element.el
                    [ Font.size 24
                    , Region.heading 2
                    ]
                    (Element.text "With automatic activation")
                , Tabs.view views
                    { tabs = List.map (toTab "automatic") allTabs
                    , active = model.activeAutomatic
                    , label = Tabs.label "Entertainment"
                    , orientation = Tabs.Horizontal
                    , activation = Tabs.Automatic
                    , onChange = UserChangedTabAutomatic
                    }
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 32
                ]
                [ Element.el
                    [ Font.size 24
                    , Region.heading 2
                    ]
                    (Element.text "With manual activation")
                , Tabs.view views
                    { tabs = List.map (toTab "manual") allTabs
                    , active = model.activeManual
                    , label = Tabs.label "Entertainment"
                    , orientation = Tabs.Horizontal
                    , activation = Tabs.Manual
                    , onChange = UserChangedTabManual
                    }
                ]
            ]
        )


toTab : String -> ( String, String ) -> Tabs.Tab (Element Msg) String
toTab id ( title, content ) =
    { tag = title
    , id = title ++ "-" ++ id
    , label =
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text title)
    , panel =
        Element.paragraph []
            [ Element.text content ]
    , focusable = True
    }


views : Tabs.Views (Element msg) msg
views =
    el
        { container =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
        , tabList =
            [ Element.width Element.fill
            , Border.widthEach
                { top = 0
                , bottom = 1
                , left = 0
                , right = 0
                }
            , Border.color gray300
            ]
        , tab =
            \active ->
                [ Element.width Element.fill
                , Element.height (Element.px 48)
                , Border.widthEach
                    { top = 0
                    , bottom = 2
                    , left = 0
                    , right = 0
                    }
                , if active then
                    Border.color primary

                  else
                    Border.color transparent
                , if active then
                    Font.color primary

                  else
                    Font.color black
                , Element.mouseDown
                    [ Font.color primaryPressed
                    , Background.color overlayPressedLight
                    ]
                , Element.mouseOver
                    [ Font.color primaryHover
                    , Background.color overlayHoverLight
                    ]
                , Element.focused
                    [ Font.color primaryFocused
                    , Background.color overlayFocusLight
                    ]
                ]
        , panel =
            always
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 8
                , Element.focused
                    [ Background.color overlayFocusLight
                    ]
                ]
        }


{-| TODO
-}
el :
    { container : List (Attribute msg)
    , tabList : List (Attribute msg)
    , tab : Bool -> List (Attribute msg)
    , panel : Bool -> List (Attribute msg)
    }
    -> Tabs.Views (Element msg) msg
el config =
    let
        attribute key value =
            Element.htmlAttribute (Html.Attributes.attribute key value)

        preventDefaultOn event decoder =
            Element.htmlAttribute (Html.Events.preventDefaultOn event decoder)

        style property value =
            Element.htmlAttribute (Html.Attributes.style property value)
    in
    Tabs.custom
        { tabs =
            \attrs { tabs, panels } ->
                let
                    withAriaLabel elAttrs =
                        case attrs.ariaLabel of
                            Nothing ->
                                elAttrs

                            Just theLabel ->
                                attribute "aria-label" theLabel :: elAttrs

                    withAriaLabelledby elAttrs =
                        case attrs.ariaLabelledBy of
                            Nothing ->
                                elAttrs

                            Just theId ->
                                attribute "aria-labelledby" theId :: elAttrs
                in
                Element.column
                    ([ attribute "role" attrs.role
                     ]
                        |> withAriaLabel
                        |> withAriaLabelledby
                        |> List.append config.container
                    )
                    [ Element.row config.tabList tabs
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        panels
                    ]
        , tab =
            \attrs { label, active } ->
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
                     , attribute "tabindex" (String.fromInt attrs.tabindex)
                     ]
                        ++ config.tab active
                    )
                    { onPress = Just attrs.onClick
                    , label = label
                    }
        , panel =
            \attrs { panel, active } ->
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
                     ]
                        |> withHidden
                        |> withTabindex
                        |> List.append (config.panel active)
                    )
                    panel
        }


primary : Color
primary =
    Element.rgb255 0 153 212


primaryHover : Color
primaryHover =
    Element.rgb255 10 157 214


primaryFocused : Color
primaryFocused =
    Element.rgb255 31 165 217


primaryPressed : Color
primaryPressed =
    Element.rgb255 82 186 226


transparent : Color
transparent =
    Element.rgba 0 0 0 0


black : Color
black =
    Element.rgb255 0 0 0


gray300 : Color
gray300 =
    Element.rgb255 224 224 224


overlayHoverLight : Color
overlayHoverLight =
    setAlpha 0.04 primary


overlayFocusLight : Color
overlayFocusLight =
    setAlpha 0.12 primary


overlayPressedLight : Color
overlayPressedLight =
    setAlpha 0.12 primary


setAlpha : Float -> Color -> Color
setAlpha alpha color_ =
    let
        rgb =
            Element.toRgb color_
    in
    Element.fromRgb
        { rgb | alpha = alpha }


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
