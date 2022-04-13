module Page.NotFound exposing (view, initPageWidget)

import Alt exposing (PageWidget, Params, RouteParser)
import Asset
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import Page.Profile exposing (Msg)



-- VIEW


view : Html Msg
view =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Not Found" ]
        , div [ class "row" ]
            [ img [ Asset.src Asset.error ] [] ]
        ]


initPageWidget : RouteParser -> PageWidget () Msg Params
initPageWidget p =
    { init = ( always ( (), Cmd.none ), p )
    , view = always view
    , update = (always << always) ( (), Cmd.none )
    , subscriptions = always Sub.none
    }
