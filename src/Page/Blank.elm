module Page.Blank exposing (initPageWidget)

import Alt exposing (PageWidget, Params, RouteParser)
import Html exposing (Html)


view : Html msg
view =
    Html.text ""


initPageWidget : RouteParser -> PageWidget () () Params
initPageWidget p =
    { init = ( always ( (), Cmd.none ), p )
    , view = always view
    , update = (always << always) ( (), Cmd.none )
    , subscriptions = always Sub.none
    }
