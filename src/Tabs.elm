module Tabs exposing
    ( viewStarter
    , view
    , Label, labelledby, label, Tab, Activation(..), Orientation(..)
    , Views, html
    , custom, TabsAttrs, TabAttrs, PanelAttrs
    )

{-|

@docs viewStarter

@docs view
@docs Label, labelledby, label, Tab, Activation, Orientation


# View customization

@docs Views, html


## Advanced customization

@docs custom, TabsAttrs, TabAttrs, PanelAttrs

-}

import Browser.Dom
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import List.Extra
import Task exposing (Task)


type alias Config node tab msg =
    { label : Label
    , tabs : List (Tab node tab)
    , active : tab
    , onChange : tab -> Task Browser.Dom.Error () -> msg
    , orientation : Orientation
    , activation : Activation
    }


{-| There are two ways to label tabs: it can be
[`labelledby`](Tabs#labelledby) by another DOM element with the given id or it
can provide its own [`label`](Tabs#label).
-}
type Label
    = Label String
    | LabelledBy String


{-| -}
labelledby : String -> Label
labelledby =
    LabelledBy


{-| -}
label : String -> Label
label =
    Label


{-|

  - **tag**: Unique tag to show the currently active tab.
  - **id**: This is used to generate CSS ids of the tab and its panel. This
    must be unique within your page.
  - **label**: Displayed label of the tab within the tab list.
  - **panel**: Content of the tab panel associated to the tab.
  - **focusable**: Indicate whether the panel itself can receive focus. Is is
    best practice to to make the panel focusable if it does not contain any other
    elements which can receive focus.

-}
type alias Tab node tag =
    { tag : tag
    , id : String
    , label : node
    , panel : node
    , focusable : Bool
    }


{-| From the [WAI-ARIA Authoring
Practices](https://w3c.github.io/aria-practices/#examples-13):

  - Tabs With `Automatic` Activation: A tabs widget where tabs are
    automatically activated and their panel is displayed when they receive focus.
  - Tabs With `Manual` Activation: A tabs widget where users activate a tab and
    display its panel by pressing Space or Enter.

-}
type Activation
    = Automatic
    | Manual


{-| -}
type Orientation
    = Horizontal
    | Vertical



---- VIEW


{-| Opaque type for providing view customization of the tabs widget.
-}
type Views node msg
    = Views
        { tabs :
            TabsAttrs
            ->
                { tabs : List node
                , panels : List node
                }
            -> node
        , tab :
            TabAttrs msg
            ->
                { label : node
                , active : Bool
                }
            -> node
        , panel :
            PanelAttrs
            ->
                { panel : node
                , active : Bool
                }
            -> node
        }


{-| Make sure to add HTML attributes for all attributes in this record to the
tabs list container.

  - **role**: The `role` HTML attribute.
  - **ariaLabel**: The `aria-label` HTML attribute.
  - **ariaLabel**: The `aria-labelledby` HTML attribute.

-}
type alias TabsAttrs =
    { role : String
    , ariaLabel : Maybe String
    , ariaLabelledBy : Maybe String
    }


{-| Make sure to add HTML attributes and event handlers for all attributes in
this record to the tab.

  - **role**: The `role` HTML attribute.
  - **ariaSelected**: The `aria-selected` HTML attribute.
  - **ariaControls**: The `aria-controls` HTML attribute.
  - **id**: The CSS id.
  - **onClick**: Attach this to an `onclick` event handler.
  - **preventDefaultOnKeydown**: Attach this to an `onkeydown` event handler
    which can prevent default.
  - **tabindex**: The `tabindex` HTML attribute.

-}
type alias TabAttrs msg =
    { role : String
    , ariaSelected : Bool
    , ariaControls : String
    , id : String
    , onClick : msg
    , preventDefaultOnKeydown : Decoder ( msg, Bool )
    , tabindex : Int
    }


{-| Make sure to add HTML attributes for all attributes in this record to the panel.

  - **role**: The `role` HTML attribute.
  - **id**: The CSS id.
  - **ariaLabelledby**: The `aria-labelledby` HTML attribute.
  - **tabindex**: The `tabindex` HTML attribute.
  - **hidden**: Is the panel hidden? One way to hide it, is by adding `display:
    none;`.

-}
type alias PanelAttrs =
    { role : String
    , id : String
    , ariaLabelledby : String
    , tabindex : Maybe Int
    , hidden : Bool
    }


{-| Render a tabs widget with default styling. You have to provide the
following fields:

  - **label**: The label of the tabs used by screen readers.
  - **tabs**: A list of all tabs with its panels. The **label** field is used
    for the entry in the tablist, as well as for tagging the currently active tab.
  - **active**: The currently active tab.
  - **onChange**: Message handler for changing the active tab. You must
    [`Task.attempt`](https://package.elm-lang.org/packages/elm/core/latest/Task#attempt)
    the second argument in your update function to make sure the correct tab
    receives focus.

**NOTE**: This function is meant to get you started with this package. If you
need more then one tabs widget on your page or want custom styling and
interaction, you should take a look at the [`view`](Tabs#view) function below.

-}
viewStarter :
    { label : String
    , tabs :
        List
            { label : String
            , panel : Html msg
            }
    , active : String
    , onChange : String -> Task Browser.Dom.Error () -> msg
    }
    -> Html msg
viewStarter config =
    let
        toTab tab =
            { tag = tab.label
            , id = tab.label
            , label = Html.text tab.label
            , panel = tab.panel
            , focusable = True
            }
    in
    view
        (Views
            { tabs =
                \tabsAttrs data ->
                    Html.div
                        []
                        [ Html.node "style" [] [ Html.text css ]
                        , htmlTabs [ Html.Attributes.class "tabs" ] [] tabsAttrs data
                        ]
            , tab = htmlTab (always [])
            , panel = htmlPanel (always [])
            }
        )
        { label = label config.label
        , tabs = List.map toTab config.tabs
        , active = config.active
        , onChange = config.onChange
        , orientation = Horizontal
        , activation = Automatic
        }


{-| Taken from <https://w3c.github.io/aria-practices/examples/tabs/css/tabs.css>
-}
css : String
css =
    """
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


{-| Render a (customized) tabs widget. You must provide [`Views`](Tabs#Views)
for rendering and the following configuration fields:

  - **label**: Specify how the tabs are labelled. See [`Label`](Tabs#Label) for
    possible options.
  - **tabs**: A list of all tabs with its panels. See [`Tab`](Tabs#Tab) for
    a description of its fields.
  - **active**: The currently active tab.
  - **onChange**: Message handler for changing the active tab. You must
    [`Task.attempt`](https://package.elm-lang.org/packages/elm/core/latest/Task#attempt)
    the second argument in your update function to make sure the correct tab
    receives focus.
  - **orientation**: Indicate if the tab list is oriented horizontally or
    vertically, this should match the actual layout.
  - **activation**: How are tabs activated? See [`Activation`](Tabs#Activation)
    for possible options.

-}
view :
    Views node msg
    ->
        { label : Label
        , tabs : List (Tab node tab)
        , active : tab
        , onChange : tab -> Task Browser.Dom.Error () -> msg
        , orientation : Orientation
        , activation : Activation
        }
    -> node
view (Views views) config =
    views.tabs
        { role = "tablist"
        , ariaLabel =
            case config.label of
                Label theLabel ->
                    Just theLabel

                LabelledBy _ ->
                    Nothing
        , ariaLabelledBy =
            case config.label of
                Label _ ->
                    Nothing

                LabelledBy theId ->
                    Just theId
        }
        { tabs = List.map (viewTab views.tab config) config.tabs
        , panels = List.map (viewPanel views.panel config.active) config.tabs
        }


viewTab :
    (TabAttrs msg
     ->
        { label : node
        , active : Bool
        }
     -> node
    )
    -> Config node tag msg
    -> Tab node tag
    -> node
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
        { role = "tab button"
        , ariaSelected = config.active == tab.tag
        , ariaControls = idPanel tab.id
        , id = idTab tab.id
        , onClick = activateTab config tab
        , preventDefaultOnKeydown =
            Json.Decode.andThen (handleKey >> preventDefault)
                (Json.Decode.field "key" Json.Decode.string)
        , tabindex =
            if config.active == tab.tag then
                0

            else
                -1
        }
        { label = tab.label
        , active = config.active == tab.tag
        }


viewPanel :
    (PanelAttrs
     ->
        { panel : node
        , active : Bool
        }
     -> node
    )
    -> tag
    -> Tab node tag
    -> node
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
        { panel = tab.panel
        , active = active == tab.tag
        }



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



---- VIEWS


{-| If you want to use other UI libraries like
[`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/)
or
[`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
you have to generate [`Views`](Tabs#Views) using this function. Take a look at
the implementation of [`html`](Tabs#html) for a starting point. The
[`examples/`](https://github.com/kirchner/elm-wai-aria-tabs/tree/main/examples)
folder of the package repository contains an implementation for
[`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).
-}
custom :
    { tabs :
        TabsAttrs
        ->
            { tabs : List node
            , panels : List node
            }
        -> node
    , tab :
        TabAttrs msg
        ->
            { label : node
            , active : Bool
            }
        -> node
    , panel :
        PanelAttrs
        ->
            { panel : node
            , active : Bool
            }
        -> node
    }
    -> Views node msg
custom config =
    Views config


{-| Generate view customization the standard
[`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) package.

The DOM structure of the tabs will be something like this:

    tabs =
        Html.div
            [ ... ] -- container attributes
            [ Html.div
                [ ... ] -- tabList attributes
                [ tabs ]
            , panels
            ]

    tab =
        Html.button
            [ ... ] -- tab attributes
            [ tabLabel ]

    panel =
        Html.div
            [ ... ] -- panel attributes
            [ panelContent ]

-}
html :
    { container : List (Attribute msg)
    , tabList : List (Attribute msg)
    , tab : Bool -> List (Attribute msg)
    , panel : Bool -> List (Attribute msg)
    }
    -> Views (Html msg) msg
html config =
    Views
        { tabs = htmlTabs config.container config.tabList
        , tab = htmlTab config.tab
        , panel = htmlPanel config.panel
        }


htmlTabs :
    List (Attribute msg)
    -> List (Attribute msg)
    -> TabsAttrs
    ->
        { tabs : List (Html msg)
        , panels : List (Html msg)
        }
    -> Html msg
htmlTabs containerAttrs tabListAttrs attrs { tabs, panels } =
    let
        withAriaLabel htmlAttrs =
            case attrs.ariaLabel of
                Nothing ->
                    htmlAttrs

                Just theLabel ->
                    ariaLabel theLabel :: htmlAttrs

        withAriaLabelledby htmlAttrs =
            case attrs.ariaLabelledBy of
                Nothing ->
                    htmlAttrs

                Just theId ->
                    ariaLabelledby theId :: htmlAttrs
    in
    Html.div
        containerAttrs
        (Html.div
            ([ role attrs.role ]
                |> withAriaLabel
                |> withAriaLabelledby
                |> List.append tabListAttrs
            )
            tabs
            :: panels
        )


htmlTab :
    (Bool -> List (Attribute msg))
    -> TabAttrs msg
    ->
        { label : Html msg
        , active : Bool
        }
    -> Html msg
htmlTab tabAttrs attrs data =
    Html.button
        ([ Html.Attributes.type_ "button"
         , role attrs.role
         , ariaSelected attrs.ariaSelected
         , ariaControls attrs.ariaControls
         , Html.Attributes.id attrs.id
         , Html.Events.onClick attrs.onClick
         , Html.Events.preventDefaultOn "keydown" attrs.preventDefaultOnKeydown
         , Html.Attributes.tabindex attrs.tabindex
         ]
            |> List.append (tabAttrs data.active)
        )
        [ data.label ]


htmlPanel :
    (Bool -> List (Attribute msg))
    -> PanelAttrs
    ->
        { panel : Html msg
        , active : Bool
        }
    -> Html msg
htmlPanel panelAttrs attrs { panel, active } =
    let
        withTabindex htmlAttrs =
            case attrs.tabindex of
                Nothing ->
                    htmlAttrs

                Just tabindex ->
                    Html.Attributes.tabindex tabindex :: htmlAttrs

        withHidden htmlAttrs =
            if attrs.hidden then
                Html.Attributes.style "display" "none" :: htmlAttrs

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
            |> List.append (panelAttrs active)
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
