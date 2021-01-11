module Tabs exposing
    ( Activation(..)
    , Config
    , Orientation(..)
    , Tab
    , view
    )

import Browser.Dom
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Task exposing (Task)


type alias Config tab msg =
    { tabs : List (Tab tab msg)
    , active : tab
    , label : String
    , orientation : Orientation
    , activation : Activation
    , onChange : tab -> Task Browser.Dom.Error () -> msg
    }


type alias Tab tag msg =
    { tag : tag
    , id : String
    , label : Html msg
    , panel : Html msg
    , focusable : Bool
    }


type Activation
    = Automatic
    | Manual


type Orientation
    = Horizontal
    | Vertical


view : Config tab msg -> Html msg
view config =
    Html.div
        [ Html.Attributes.class "tabs" ]
        (Html.div
            [ role "tablist"
            , ariaLabel config.label
            ]
            (List.map (viewTab config) config.tabs)
            :: List.map (viewPanel config.active) config.tabs
        )


viewTab : Config tag msg -> Tab tag msg -> Html msg
viewTab config tab =
    let
        withTabindex attrs =
            if config.active == tab.tag then
                attrs

            else
                Html.Attributes.tabindex -1 :: attrs

        handleKey key =
            case key of
                "Enter" ->
                    Just (activateTab config tab)

                " " ->
                    Just (activateTab config tab)

                "ArrowRight" ->
                    case config.activation of
                        Automatic ->
                            activateNextTab config tab.id

                        Manual ->
                            focusNextTab config tab.id

                "ArrowLeft" ->
                    case config.activation of
                        Automatic ->
                            activatePreviousTab config tab.id

                        Manual ->
                            focusPreviousTab config tab.id

                "Home" ->
                    case config.activation of
                        Automatic ->
                            activateFirstTab config

                        Manual ->
                            focusFirstTab config

                "End" ->
                    case config.activation of
                        Automatic ->
                            activateLastTab config

                        Manual ->
                            focusLastTab config

                _ ->
                    Nothing

        preventDefault =
            Maybe.map (\msg -> Json.Decode.succeed ( msg, True ))
                >> Maybe.withDefault (Json.Decode.fail "not handling that key here")
    in
    Html.button
        ([ Html.Attributes.type_ "button"
         , role "tab"
         , ariaSelected (config.active == tab.tag)
         , ariaControls (idPanel tab.id)
         , Html.Attributes.id (idTab tab.id)
         , Html.Events.onClick (activateTab config tab)
         , Html.Events.preventDefaultOn "keydown"
            (Json.Decode.andThen (handleKey >> preventDefault)
                (Json.Decode.field "key" Json.Decode.string)
            )
         ]
            |> withTabindex
        )
        [ tab.label ]


viewPanel : tag -> Tab tag msg -> Html msg
viewPanel active tab =
    let
        withTabindex attrs =
            if tab.focusable then
                Html.Attributes.tabindex 0 :: attrs

            else
                attrs

        withHidden attrs =
            if active == tab.tag then
                attrs

            else
                -- TODO hide
                Html.Attributes.class "is-hidden" :: attrs
    in
    Html.div
        ([ role "tabpanel"
         , Html.Attributes.id (idPanel tab.id)
         , ariaLabelledby (idTab tab.id)
         ]
            |> withTabindex
            |> withHidden
        )
        [ tab.panel ]



---- TASKS


focusFirstTab : Config tab msg -> Maybe msg
focusFirstTab config =
    List.head config.tabs
        |> Maybe.map (focusTab config)


activateFirstTab : Config tag msg -> Maybe msg
activateFirstTab config =
    List.head config.tabs
        |> Maybe.map (activateTab config)


focusLastTab : Config tab msg -> Maybe msg
focusLastTab config =
    focusFirstTab { config | tabs = List.reverse config.tabs }


activateLastTab : Config tag msg -> Maybe msg
activateLastTab config =
    activateFirstTab { config | tabs = List.reverse config.tabs }


focusNextTab : Config tab msg -> String -> Maybe msg
focusNextTab config focused =
    next config.tabs focused
        |> Maybe.map (focusTab config)


activateNextTab : Config tab msg -> String -> Maybe msg
activateNextTab config focused =
    next config.tabs focused
        |> Maybe.map (activateTab config)


focusPreviousTab : Config tab msg -> String -> Maybe msg
focusPreviousTab config focused =
    focusNextTab { config | tabs = List.reverse config.tabs } focused


activatePreviousTab : Config tab msg -> String -> Maybe msg
activatePreviousTab config focused =
    activateNextTab { config | tabs = List.reverse config.tabs } focused


focusTab : Config tag msg -> Tab tag msg -> msg
focusTab config tab =
    config.onChange config.active (Browser.Dom.focus (idTab tab.id))


activateTab : Config tag msg -> Tab tag msg -> msg
activateTab config tab =
    config.onChange tab.tag (Browser.Dom.focus (idTab tab.id))



---- HELP


next : List (Tab tag msg) -> String -> Maybe (Tab tag msg)
next tabs id =
    case List.Extra.dropWhile (\tab -> tab.id /= id) tabs of
        _ :: tab :: _ ->
            Just tab

        _ ->
            List.head tabs



---- IDS


idPanel : String -> String
idPanel id =
    id ++ "--tab-panel"


idTab : String -> String
idTab id =
    id ++ "--tab"



---- HELPER


role : String -> Attribute msg
role =
    Html.Attributes.attribute "role"


ariaLabel : String -> Attribute msg
ariaLabel =
    Html.Attributes.attribute "aria-label"


ariaLabelledby : String -> Attribute msg
ariaLabelledby =
    Html.Attributes.attribute "aria-labelledby"


ariaSelected : Bool -> Attribute msg
ariaSelected bool =
    Html.Attributes.attribute "aria-selected"
        (if bool then
            "true"

         else
            "false"
        )


ariaControls : String -> Attribute msg
ariaControls =
    Html.Attributes.attribute "aria-controls"
