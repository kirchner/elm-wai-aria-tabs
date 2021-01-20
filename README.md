# elm-wai-aria-tabs

This package offers an implementation of the [Tabs
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#tabpanel) as specified in
the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/):

> Tabs are a set of layered sections of content, known as tab panels, that
> display one panel of content at a time. Each tab panel has an associated tab
> element, that when activated, displays the panel. The list of tab elements is
> arranged along one edge of the currently displayed panel, most commonly the top
> edge.

Take a look at the [demo page](https://kirchner.github.io/elm-wai-aria-tabs/).

See more end-to-end example code in the
[`examples/`](https://github.com/kirchner/elm-wai-aria-tabs/tree/main/examples)
folder.


## Design Goals

- Stick to the WAI-ARIA guideline as close as possible.
- Offer a simple version for the standard
  [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) package.
- Make the widget customizable for any other UI library, e.g.
  [rtfeldman/elm-css](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/)
  or
  [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).


## Overview

The minimal code to get working tabs with automatic activation would be
something like this:

```elm
import Browser.Dom
import Html exposing (Html)
import Accessibility.Tabs
import Task exposing (Task)



type alias Model =
    { active : String }


type Msg
    = NoOp
    | UserChangedTab String (Task Browser.Dom.Error ())


init : ( Model, Cmd Msg )
init =
    ( { active = "Nils Frahm" }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UserChangedTab active task ->
            ( { model | active = active }
            , Task.attempt (\_ -> NoOp) task
            )


view : Model -> Html Msg
view model =
    let
        toTab ( title, content ) =
            { label = title
            , panel = Html.text content
            }
    in
    Accessibility.Tabs.viewStarter
        { tabs = List.map toTab tabs
        , active = model.active
        , label = "Entertainment"
        , onChange = UserChangedTab
        }


tabs : List ( String, String )
tabs =
    [ ( "Nils Frahm"
      , "Nils Frahm is a ..."
      )
    , ...
    ]
```


## Usage

Add the
[kirchner/elm-wai-aria-tabs](https://package.elm-lang.org/packages/kirchner/elm-wai-aria-tabs/latest/)
Elm package as a dependency by running

```
$ elm install kirchner/elm-wai-aria-tabs
```
