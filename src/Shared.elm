module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , onEnter
    , parseFragmentToSpeedDial
    , speedDialToLabel
    , speedDialToElement
    , FetchState(..)
    , labelWithSpinner
    , view
    , stringToManualSpeedDial
    , loginForm
    )

import Attr
import Base64
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Route as Route
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import OdorikApi
import Request exposing (Request)
import Storage exposing (Storage)
import Url
import View exposing (View)


type alias Flags =
    { storage : Json.Value
    , windowHeight : Int
    , windowWidth : Int
    }


type alias Model =
    { storage : Storage
    , width : Int
    , height : Int
    }

type alias IncomingFragment =
    { name : String
    , number : String
    , label : String
    , version : Int
    }

type FetchState
    = Fetching
    | Idle
    | Error String
    | Success

init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    (   { storage = Storage.fromJson flags.storage
        , width = flags.windowWidth
        , height = flags.windowHeight
        }
    , Cmd.none
    )


type Msg
    = StorageUpdated Storage
    | WindowResized Int Int


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )
        WindowResized w h ->
            ( { model | width = w, height = h }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Storage.onChange StorageUpdated
        , onResize WindowResized
        ]

menuHeight : Int
menuHeight = 40

pageMaxWidth : Int
pageMaxWidth = 800

view : Model -> Request -> String -> Element msg -> View msg
view m req title contents =
    { title = title ++ " :: Odorik Callback"
    , attributes =
        [ width <| minimum m.width (maximum pageMaxWidth fill)
        , height <| minimum m.height fill
        , inFront <| menuGen req
        ]
    , element =
        el
            [ width <| maximum pageMaxWidth fill
            , centerX
            , centerY
            , padding 10 ] <|
            column [ width fill, height fill ]
                [ row [ width fill, height <| px (menuHeight + 20) ] [ text "" ]
                , contents
                ]
    }

menuGen : Request -> Element msg
menuGen req =
    row
        [ centerX
        , width <| maximum pageMaxWidth fill
        , padding 10
        , spacing 10
        , Border.rounded 8
        , Background.color <| rgb255 240 240 240
        ]
        [ link [ width <| fillPortion 1 ]
            { url = (Route.toHref Route.Home_)
            , label = el
                -- "logo" element
                []
                <| image [ Border.rounded 8, clip, width <| px menuHeight, height <| px menuHeight ]
                    { src = "icon.png"
                    , description = "odorik icon"
                    } }
        , link [ width <| fillPortion 1, alignRight ]
            { url = (Route.toHref Route.Balance)
            , label = el [ width <| fillPortion 1, alignRight ] <| text "Balance" }
        , link [ width <| fillPortion 1, alignRight ]
            { url = (Route.toHref Route.Callback)
            , label = el [ width <| fillPortion 1, alignRight ] <| text "Callback" }
        , link [ width <| fillPortion 1, alignRight ]
            { url = (Route.toHref Route.Settings)
            , label = el [ width <| fillPortion 1, alignRight ] <| text "Settings" }
        ]

onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.field "key" Json.string
                |> Json.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.succeed msg
                        else
                            Json.fail "Not the enter key"
                    )
            )
        )

incomingFragmentDecoder : Json.Decoder IncomingFragment
incomingFragmentDecoder =
    Json.map4 IncomingFragment
        (Json.field "name" Json.string)
        (Json.field "number" Json.string)
        (Json.field "label" Json.string)
        (Json.field "version" Json.int)

fragmentToSpeedDial : IncomingFragment -> Result String (Maybe OdorikApi.SpeedDial)
fragmentToSpeedDial f =
    case f.version of
        1 -> Ok <| Just { shortcut = 0, number = f.number, name = f.name ++ " (" ++ f.label ++ ")" }
        _ -> Err <| "unsupported record version: " ++ String.fromInt f.version

parseIncomingJson : String -> Result String (Maybe OdorikApi.SpeedDial)
parseIncomingJson rec =
    case Json.decodeString incomingFragmentDecoder rec of
        Err e -> Err <| Json.errorToString e
        Ok f -> fragmentToSpeedDial f

parseFragmentToSpeedDial : Url.Url -> Result String (Maybe OdorikApi.SpeedDial)
parseFragmentToSpeedDial url =
    case url.fragment of
        Just str -> case (Base64.decode str) of
            Ok rec -> parseIncomingJson rec
            Err _ -> Err "invalid encoding"
        Nothing -> Ok Nothing

speedDialToLabel : OdorikApi.SpeedDial -> String
speedDialToLabel item =
    case item.shortcut of
        0 -> item.name
        _ -> "(#" ++ String.fromInt item.shortcut ++ ") " ++ item.name

speedDialToElement : List (Attribute msg) -> OdorikApi.SpeedDial -> Element msg
speedDialToElement attr item =
    column [ width fill ]
        [ el
            (
                [ padding 8
                , spacing 10
                , width fill
                , Font.size 20
                ]
            ++ attr
            )
            (text <| speedDialToLabel item)
        , el
            (
                [ padding 8
                , spacing 10
                , width fill
                , Font.size 16
                ]
            ++ attr
            )
            (text item.number)
        ]

stringToManualSpeedDial : String -> OdorikApi.SpeedDial
stringToManualSpeedDial s =
    { shortcut = 0
    , number = s
    , name = "Manual entry"
    }

labelWithSpinner : FetchState -> String -> Maybe msg -> List (Element msg)
labelWithSpinner state label reloadMsg =
    case state of
        Fetching ->
            [ paragraph [ Font.alignLeft ] [ text label ]
            , paragraph [ Font.alignRight ] [ Attr.spinnerAnimatedIcon 20 20 ]
            ]
        Idle ->
            case reloadMsg of
                Nothing ->
                    [ paragraph [ Font.alignLeft ] [ text label ]
                    ]
                Just a ->
                    [ paragraph [ Font.alignLeft ] [ text label ]
                    , Input.button Attr.linkLikeButton
                        { onPress = Just a
                        , label = Attr.spinnerIcon 20 20
                        }
                    ]
        Error e ->
            case reloadMsg of
                Nothing ->
                    [ paragraph [ Font.alignLeft ] [ text label ]
                    , paragraph ( Font.alignRight :: Attr.error ) [ text e ]
                    , paragraph [ Font.alignRight ] [ Attr.crossIcon 20 20 ]
                    ]
                Just a ->
                    [ paragraph [ Font.alignLeft ] [ text label ]
                    , paragraph ( Font.alignRight :: Attr.error ) [ text e ]
                    , Input.button Attr.linkLikeButton
                        { onPress = Just a
                        , label = Attr.crossIcon 20 20
                        }
                    ]
        Success ->
            [ paragraph [ Font.alignLeft ] [ text label ]
            , paragraph [ Font.alignRight ] [ Attr.checkmarkIcon 20 20 ]
            ]

loginForm : msg -> List (Element msg)
loginForm m =
    let
        title = "Not logged in."
        button = { label = text "Login", onPress = Just m }
    in
        [ paragraph
            [Font.size 48, Font.center]
            [ text title ]
        , paragraph
            [Font.size 24, Font.center]
            [ text "" ]
        , paragraph
            [ Font.center ]
            [ Input.button
                Attr.button
                button
            ]
        ]
