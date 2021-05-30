module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , menuGen
    , onEnter
    , parseFragmentToSpeedDial
    , speedDialToLabel
    , speedDialToElement
    , FetchState(..)
    , labelWithSpinner
    )

import Base64
import Element exposing (..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Gen.Route as Route
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import OdorikApi
import Request exposing (Request)
import Storage exposing (Storage)
import Url


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    }

type alias IncomingFragment =
    { name : String
    , number : String
    , label : String
    , version : Int
    }

type FetchState
    = Fetching
    | Ready
    | Error String

init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { storage = Storage.fromJson flags }
    , Cmd.none
    )


type Msg
    = StorageUpdated Storage


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange StorageUpdated


menuGen : Request -> Element msg
menuGen req =
    row
        [ centerX
        , width <| maximum 800 fill
        , padding 10
        , spacing 10
        , Background.color <| rgb255 255 255 255
        ]
        [ link [ width <| fillPortion 1 ]
            { url = (Route.toHref Route.Home_)
            , label = el
                -- "logo" element
                []
                <| image [ Border.rounded 8, clip, width <| px 80, height <| px 80 ]
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

labelWithSpinner : FetchState -> String -> List (Element msg)
labelWithSpinner s t =
    case s of
        Fetching ->
            [ paragraph [ Font.alignLeft ] [ text t ]
            , paragraph [ Font.alignRight ] [ text "(loading...)" ]
            ]
        Ready ->
            [ paragraph [ Font.alignLeft ] [ text t ]
            ]
        Error e ->
            [ paragraph [ Font.alignLeft ] [ text t ]
            , paragraph [ Font.alignRight ] [ text <| "(error: " ++ e ++ ")" ]
            ]
