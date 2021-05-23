module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , menuGen
    , apiUrlFromCreds
    )

import Element exposing (..)
import Element.Border as Border
import Element.Background as Background
import Gen.Route as Route
import Json.Decode as Json
import Request exposing (Request)
import Storage exposing (Storage)


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    }


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

apiUrlFromCreds : Storage -> String -> String
apiUrlFromCreds storage api =
    case (storage.user, storage.pass) of
        ("888", "888") -> ("https://wejn.com/odorik_test_api___/" ++ api ++ "?user=" ++ storage.user ++ "&password=" ++ storage.pass)
        _ -> ("https://www.odorik.cz/api/v1/" ++ api ++ "?user=" ++ storage.user ++ "&password=" ++ storage.pass)
        -- FIXME: this is SO bad, but it's a start
