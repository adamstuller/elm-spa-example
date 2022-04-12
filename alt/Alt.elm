module Alt exposing
    ( Both
    , Subscription
    , Update
    , View
    , Flags
    , ApplicationWithRouter, Navbar, NavbarState, PageWidget, PageWidgetComposition, Params, RouteParser, add, basicParser, emptyNavbar, initRouter, initWith, join, oneOfInits, orInit, subscribeEither, subscribeWith, updateEither, updateWith, viewEither
    )

{-| This module contains basic types for elm architecture functions


# Both

@docs Both


# Subscription

@docs Subscription


# Update

@docs Update


# View

@docs View


# Route

@docs Route


# Flags

@docs Flags

-}

import Browser exposing (UrlRequest)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Either exposing (Either(..))
import Html exposing (Html)
import Html.Attributes exposing (default, height, href, width)
import Json.Decode exposing (Decoder, decodeValue, field, int, map2)
import List.Nonempty as NE exposing (Nonempty)
import Url
import Url.Parser exposing (Parser)


{-| Subscription function type
-}
type alias Subscription model msg =
    model -> Sub msg


{-| Update function type
-}
type alias Update model msg =
    msg -> model -> ( model, Cmd msg )


{-| View function type
-}
type alias View model msg =
    model -> Html msg


{-| Type representing route, for now just String
-}
type alias RouteParser =
    Parser (List String -> List String) (List String)


basicParser : String -> Parser (List String -> List String) (List String)
basicParser s =
    Url.Parser.map [] (Url.Parser.s s)


{-| Simple type used for better readability of tuple

    Both a b == ( a, b )

-}
type alias Both a b =
    ( a, b )


{-| Flags type used in the router
-}
type alias Flags =
    Json.Decode.Value


type alias Params =
    { flags : Flags
    , url : Url.Url
    , urlParams : List String
    , key: Nav.Key
    }


{-| Starts the combination of init functions. Adding another init function is done by orInit function.
-}
oneOfInits :
    Both (flags -> ( model1, Cmd msg1 )) RouteParser
    -> Both (flags -> ( model2, Cmd msg2 )) RouteParser
    -> ( Either () () -> flags -> ( Either model1 model2, Cmd (Either msg1 msg2) ), Nonempty (Either () ()), Nonempty RouteParser )
oneOfInits ( init1, route1 ) ( init2, route2 ) =
    ( \path ->
        case path of
            Left () ->
                \flags -> Tuple.mapBoth Left (Cmd.map Left) <| init1 flags

            Right () ->
                \flags -> Tuple.mapBoth Right (Cmd.map Right) <| init2 flags
    , NE.cons (Left ()) <| NE.fromElement (Right ())
    , NE.cons route1 <| NE.fromElement route2
    )


{-| Adds an another init to init composition
-}
orInit :
    ( path -> flags -> ( model1, Cmd msg1 ), Nonempty path, Nonempty RouteParser )
    -> Both (flags -> ( model2, Cmd msg2 )) RouteParser
    -> ( Either path () -> flags -> ( Either model1 model2, Cmd (Either msg1 msg2) ), Nonempty (Either path ()), Nonempty RouteParser )
orInit ( next, ps, routes ) ( init2, route2 ) =
    ( \path ->
        case path of
            Left p ->
                \flags ->
                    Tuple.mapBoth Left (Cmd.map Left) <| next p flags

            Right () ->
                \flags ->
                    Tuple.mapBoth Right (Cmd.map Right) <| init2 flags
    , NE.append
        (NE.map Left ps)
      <|
        NE.fromElement (Right ())
    , NE.append routes <|
        NE.fromElement route2
    )


{-| Combines two init functions.
-}
initWith :
    (flags -> ( model1, Cmd msg1 ))
    -> (flags -> ( model2, Cmd msg2 ))
    -> (flags -> ( Both model1 model2, Cmd (Either msg1 msg2) ))
initWith init1 init2 flags =
    let
        ( m2, c2 ) =
            init2 flags

        ( m1, c1 ) =
            init1 flags
    in
    ( ( m1, m2 )
    , Cmd.batch
        [ Cmd.map Left c1
        , Cmd.map Right c2
        ]
    )



--- Update Composition


{-| Is composition of two update functions creating sum of their models. This function updates one of the submodels.
-}
updateEither :
    Update model1 msg1
    -> Update model2 msg2
    -> Update (Either model1 model2) (Either msg1 msg2)
updateEither u1 u2 msg model =
    case ( msg, model ) of
        ( Left msg1, Left model1 ) ->
            Tuple.mapBoth Left (Cmd.map Left) <| u1 msg1 model1

        ( Right msg2, Right model2 ) ->
            Tuple.mapBoth Right (Cmd.map Right) <| u2 msg2 model2

        _ ->
            ( model, Cmd.none )


{-| Is composition of two update functions creating tuple of their models. This function updates one of the submodels.
-}
updateWith :
    Update model1 msg1
    -> Update model2 msg2
    -> Update (Both model1 model2) (Either msg1 msg2)
updateWith u1 u2 =
    let
        applyL f m =
            Tuple.mapFirst (f m)
                >> (\( ( x, c ), y ) -> ( ( x, y ), Cmd.map Left c ))

        applyR f m =
            Tuple.mapSecond (f m)
                >> (\( x, ( y, c ) ) -> ( ( x, y ), Cmd.map Right c ))
    in
    Either.unpack (applyL u1) (applyR u2)



--- Subscribe composition


{-| Combines two subscriptions creating tuple of models.
-}
subscribeWith :
    Subscription model1 msg1
    -> Subscription model2 msg2
    -> Subscription (Both model1 model2) (Either msg1 msg2)
subscribeWith s1 s2 ( m1, m2 ) =
    Sub.batch
        [ Sub.map Left <| s1 m1
        , Sub.map Right <| s2 m2
        ]


{-| Combines two subscriptions creating sum of models.
-}
subscribeEither :
    Subscription model1 msg1
    -> Subscription model2 msg2
    -> Subscription (Either model1 model2) (Either msg1 msg2)
subscribeEither s1 s2 model =
    case model of
        Left model1 ->
            Sub.map Left <| s1 model1

        Right model2 ->
            Sub.map Right <| s2 model2



--- View composition


{-| View composition expecting tuple of models
-}
viewEither :
    View model1 msg1
    -> View model2 msg2
    -> View (Either model1 model2) (Either msg1 msg2)
viewEither v1 v2 m =
    case m of
        Left m1 ->
            Html.map Left <| v1 m1

        Right m2 ->
            Html.map Right <| v2 m2


{-| Type for widet that represents page. Contains all basic elm architecture functions that need to be implemented in respective page modules.
-}
type alias PageWidget model msg flags =
    { init : Both (flags -> ( model, Cmd msg )) RouteParser
    , view : View model msg
    , update : Update model msg
    , subscriptions : Subscription model msg
    }


{-| Page composition in progress. Is created by join function.
-}
type alias PageWidgetComposition model msg path flags =
    { init : ( path -> flags -> ( model, Cmd msg ), Nonempty path, Nonempty RouteParser )
    , view : View model msg
    , update : Update model msg
    , subscriptions : Subscription model msg
    }


{-| Parameter for single page applications. Is created by attaching router to PageWidgetComposition
-}
type alias ApplicationWithRouter model msg flags =
    { init : flags -> Url.Url -> Nav.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : Update model msg
    , subscriptions : Subscription model msg
    , onUrlChange : Url.Url -> msg
    , onUrlRequest : UrlRequest -> msg
    }


{-| Combines first two pages and creates PageWidgetComposition. Is followed by add function.
-}
join : PageWidget model1 msg1 flags -> PageWidget model2 msg2 flags -> PageWidgetComposition (Either model1 model2) (Either msg1 msg2) (Either () ()) flags
join w1 w2 =
    { init = oneOfInits w1.init w2.init
    , view = viewEither w1.view w2.view
    , update = updateEither w1.update w2.update
    , subscriptions = subscribeEither w1.subscriptions w2.subscriptions
    }


{-| Adds another PageWidget to PageWidgetComposition.
-}
add : PageWidgetComposition model1 msg1 path flags -> PageWidget model2 msg2 flags -> PageWidgetComposition (Either model1 model2) (Either msg1 msg2) (Either path ()) flags
add w1 w2 =
    { init = orInit w1.init w2.init
    , view = viewEither w1.view w2.view
    , update = updateEither w1.update w2.update
    , subscriptions = subscribeEither w1.subscriptions w2.subscriptions
    }


flagsDecoder : Decoder Window
flagsDecoder =
    map2 Window
        (field "width" int)
        (field "heiht" int)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , flags : Flags
    , navbarState : NavbarState
    }


type alias Window =
    { width : Int
    , height : Int
    }


{-| State that navbar expects. When implementing custom navbar, this state can be accessed.
Contains window to determine the shape of navbar and information whether it is collapsed or not.
-}
type alias NavbarState =
    { window : Window
    , expanded : Bool
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | WindowSizeChanged Window
    | NavbarExpandedClicked


{-| Navbar type. Function that returns view of navbar.
-}
type alias Navbar msg =
    NavbarState -> msg -> Url.Url -> Html msg


{-| Simplest empty navbar
-}
emptyNavbar : Navbar msg
emptyNavbar navbarState onNavbarExpandClicked url =
    Html.div [] []


pathFromUrl : Nonempty ( path, RouteParser ) -> Url.Url -> path
pathFromUrl rules url =
    let
        default =
            NE.head rules

        matchesUrl u ( p, parser ) =
            case Url.Parser.parse parser u of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    NE.filter (matchesUrl url) default rules
        |> NE.head
        |> Tuple.first


paramsFromUrl : Nonempty ( path, RouteParser ) -> Url.Url -> List String
paramsFromUrl rules url =
    let
        default =
            NE.head rules

        matchesUrl u ( p, parser ) =
            case Url.Parser.parse parser u of
                Just _ ->
                    True

                Nothing ->
                    False

        routeParser =
            NE.filter (matchesUrl url) default rules
                |> NE.head
                |> Tuple.second
    in
    Maybe.withDefault [] <| Url.Parser.parse routeParser url


routerSubscriptions : Model -> Sub Msg
routerSubscriptions model =
    onResize <|
        \width height ->
            WindowSizeChanged { width = width, height = height }


{-| This function takes PageWidgetComposition and another information about aplications. It attaches router to composition and creates suitable output for application.
-}
initRouter :
    String
    -> Navbar Msg
    -> PageWidgetComposition model msg path Params
    -> ApplicationWithRouter (Both model Model) (Either msg Msg) Flags
initRouter title n w =
    let
        ( select, paths, routes ) =
            w.init

        routingRules =
            NE.zip paths routes

        update =
            \msg ( models, { url, key, flags, navbarState } ) ->
                case msg of
                    Left subMsg ->
                        let
                            ( subModel, subCmd ) =
                                w.update subMsg models
                        in
                        ( ( subModel, Model key url flags navbarState ), Cmd.map Left subCmd )

                    Right routerMsg ->
                        case routerMsg of
                            LinkClicked urlRequest ->
                                case urlRequest of
                                    Browser.Internal internalUrl ->
                                        ( ( models, Model key internalUrl flags navbarState )
                                        , Cmd.map Right <|
                                            Nav.pushUrl key (Url.toString internalUrl)
                                        )

                                    Browser.External href ->
                                        ( ( models, Model key url flags navbarState )
                                        , Cmd.map Right <| Nav.load href
                                        )

                            UrlChanged newUrl ->
                                let
                                    ( subModel, subCmd ) =
                                        select (pathFromUrl routingRules newUrl) { flags = flags, url = newUrl, urlParams = paramsFromUrl routingRules newUrl, key = key }
                                in
                                ( ( subModel, Model key url flags navbarState )
                                , Cmd.map Left subCmd
                                )

                            WindowSizeChanged window ->
                                let
                                    ( subModel, subCmd ) =
                                        select (pathFromUrl routingRules url) { flags = flags, url = url, urlParams = paramsFromUrl routingRules url, key = key }

                                    newNavbarState =
                                        { navbarState | window = window }
                                in
                                ( ( subModel, Model key url flags newNavbarState )
                                , Cmd.map Left subCmd
                                )

                            NavbarExpandedClicked ->
                                let
                                    ( subModel, subCmd ) =
                                        select (pathFromUrl routingRules url) { flags = flags, url = url, urlParams = paramsFromUrl routingRules url, key = key }

                                    newNavbarState =
                                        { navbarState | expanded = not navbarState.expanded }
                                in
                                ( ( subModel, Model key url flags newNavbarState )
                                , Cmd.map Left subCmd
                                )

        init flags url key =
            let
                ( model, cmd ) =
                    select (pathFromUrl routingRules url) { flags = flags, url = url, urlParams = paramsFromUrl routingRules url, key = key }

                window =
                    case decodeValue flagsDecoder flags of
                        Ok decodedWindow ->
                            decodedWindow

                        Err _ ->
                            { width = 1200, height = 800 }

                navbarState =
                    { window = window, expanded = False }
            in
            ( ( model, Model key url flags navbarState ), Cmd.map Left cmd )

        view =
            \( models, { url, navbarState } ) ->
                { title = title
                , body =
                    [ Html.div []
                        [ Html.map Right <| n navbarState NavbarExpandedClicked url
                        , Html.map Left <| w.view models
                        ]
                    ]
                }

        subscriptions =
            subscribeWith w.subscriptions routerSubscriptions
    in
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = \url -> Right <| UrlChanged url
    , onUrlRequest = \urlRequest -> Right <| LinkClicked urlRequest
    }
