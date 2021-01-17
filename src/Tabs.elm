module Tabs exposing
    ( Config, Tab, Activation(..), Orientation(..), view
    , Views, TabsAttrs, TabAttrs, PanelAttrs, viewCustom
    )

{-|

@docs Config, Tab, Activation, Orientation, view
@docs Views, TabsAttrs, TabAttrs, PanelAttrs, viewCustom

-}

import Browser.Dom
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import List.Extra
import Task exposing (Task)


{-| TODO
-}
type alias Config node tab msg =
    { tabs : List (Tab node tab)
    , active : tab
    , label : String
    , orientation : Orientation
    , activation : Activation
    , onChange : tab -> Task Browser.Dom.Error () -> msg
    }


{-| TODO
-}
type alias Tab node tag =
    { tag : tag
    , id : String
    , label : node
    , panel : node
    , focusable : Bool
    }


{-| TODO
-}
type Activation
    = Automatic
    | Manual


{-| TODO
-}
type Orientation
    = Horizontal
    | Vertical



---- VIEW


{-| TODO
-}
type alias Views node msg =
    { tabs : TabsAttrs -> List node -> List node -> node
    , tab : TabAttrs msg -> node -> Bool -> node
    , panel : PanelAttrs -> node -> node
    }


{-| TODO
-}
type alias TabsAttrs =
    { role : String
    , ariaLabel : String
    }


{-| TODO
-}
type alias TabAttrs msg =
    { role : String
    , ariaSelected : Bool
    , ariaControls : String
    , id : String
    , onClick : msg
    , preventDefaultOnKeydown : Decoder ( msg, Bool )
    , tabindex : Maybe Int
    }


{-| TODO
-}
type alias PanelAttrs =
    { role : String
    , id : String
    , ariaLabelledby : String
    , tabindex : Maybe Int
    , hidden : Bool
    }


{-| TODO
-}
viewCustom : Views node msg -> Config node tab msg -> node
viewCustom views config =
    views.tabs
        { role = "tablist"
        , ariaLabel = config.label
        }
        (List.map (viewTab views.tab config) config.tabs)
        (List.map (viewPanel views.panel config.active) config.tabs)


viewTab : (TabAttrs msg -> node -> Bool -> node) -> Config node tag msg -> Tab node tag -> node
viewTab toNode config tab =
    let
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
    toNode
        { role = "tab"
        , ariaSelected = config.active == tab.tag
        , ariaControls = idPanel tab.id
        , id = idTab tab.id
        , onClick = activateTab config tab
        , preventDefaultOnKeydown =
            Json.Decode.andThen (handleKey >> preventDefault)
                (Json.Decode.field "key" Json.Decode.string)
        , tabindex =
            if config.active == tab.tag then
                -- FIXME according to spec we want 'Nothing' here, but then the
                -- elm-ui version will not work, since elm-ui buttons are
                -- implemented as div's with role="button"
                Just 0

            else
                Just -1
        }
        tab.label
        (config.active == tab.tag)


viewPanel : (PanelAttrs -> node -> node) -> tag -> Tab node tag -> node
viewPanel toNode active tab =
    toNode
        { role = "tabpanel"
        , id = idPanel tab.id
        , ariaLabelledby = idTab tab.id
        , tabindex =
            if tab.focusable then
                Just 0

            else
                Nothing
        , hidden = active /= tab.tag
        }
        tab.panel



---- TASKS


focusFirstTab : Config node tab msg -> Maybe msg
focusFirstTab config =
    List.head config.tabs
        |> Maybe.map (focusTab config)


activateFirstTab : Config node tag msg -> Maybe msg
activateFirstTab config =
    List.head config.tabs
        |> Maybe.map (activateTab config)


focusLastTab : Config node tab msg -> Maybe msg
focusLastTab config =
    focusFirstTab { config | tabs = List.reverse config.tabs }


activateLastTab : Config node tag msg -> Maybe msg
activateLastTab config =
    activateFirstTab { config | tabs = List.reverse config.tabs }


focusNextTab : Config node tab msg -> String -> Maybe msg
focusNextTab config focused =
    next config.tabs focused
        |> Maybe.map (focusTab config)


activateNextTab : Config node tab msg -> String -> Maybe msg
activateNextTab config focused =
    next config.tabs focused
        |> Maybe.map (activateTab config)


focusPreviousTab : Config node tab msg -> String -> Maybe msg
focusPreviousTab config focused =
    focusNextTab { config | tabs = List.reverse config.tabs } focused


activatePreviousTab : Config node tab msg -> String -> Maybe msg
activatePreviousTab config focused =
    activateNextTab { config | tabs = List.reverse config.tabs } focused


focusTab : Config node tag msg -> Tab node tag -> msg
focusTab config tab =
    config.onChange config.active (Browser.Dom.focus (idTab tab.id))


activateTab : Config node tag msg -> Tab node tag -> msg
activateTab config tab =
    config.onChange tab.tag (Browser.Dom.focus (idTab tab.id))


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



---- HTML


{-| TODO
-}
view : Config (Html msg) tab msg -> Html msg
view =
    viewCustom
        { tabs = htmlTabs
        , tab = htmlTab
        , panel = htmlPanel
        }


htmlTabs : TabsAttrs -> List (Html msg) -> List (Html msg) -> Html msg
htmlTabs attrs tabs panels =
    Html.div
        [ Html.Attributes.class "tabs" ]
        (Html.div
            [ role attrs.role
            , ariaLabel attrs.ariaLabel
            ]
            tabs
            :: panels
        )


htmlTab : TabAttrs msg -> Html msg -> Bool -> Html msg
htmlTab attrs label _ =
    let
        withTabindex htmlAttrs =
            case attrs.tabindex of
                Nothing ->
                    htmlAttrs

                Just tabindex ->
                    Html.Attributes.tabindex tabindex :: htmlAttrs
    in
    Html.button
        ([ Html.Attributes.type_ "button"
         , role attrs.role
         , ariaSelected attrs.ariaSelected
         , ariaControls attrs.ariaControls
         , Html.Attributes.id attrs.id
         , Html.Events.onClick attrs.onClick
         , Html.Events.preventDefaultOn "keydown" attrs.preventDefaultOnKeydown
         ]
            |> withTabindex
        )
        [ label ]


htmlPanel : PanelAttrs -> Html msg -> Html msg
htmlPanel attrs panel =
    let
        withTabindex htmlAttrs =
            case attrs.tabindex of
                Nothing ->
                    htmlAttrs

                Just tabindex ->
                    Html.Attributes.tabindex tabindex :: htmlAttrs

        withHidden htmlAttrs =
            if attrs.hidden then
                -- TODO hide
                Html.Attributes.class "is-hidden" :: htmlAttrs

            else
                htmlAttrs
    in
    Html.div
        ([ role attrs.role
         , Html.Attributes.id attrs.id
         , ariaLabelledby attrs.ariaLabelledby
         ]
            |> withTabindex
            |> withHidden
        )
        [ panel ]



---- HTML ATTRIBUTES


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
