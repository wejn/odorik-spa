port module Storage exposing
    ( Storage, fromJson, onChange
    , logout
    , login
    , saveCaller
    , saveLine
    , saveSpeedDials
    )

{-|

@docs Storage, fromJson, onChange
@docs logout, login
@docs saveCaller, saveLine

-}

import Json.Decode as Json
import Json.Encode as Encode
import OdorikApi
import Task



-- PORTS


port save : Json.Value -> Cmd msg


port load : (Json.Value -> msg) -> Sub msg



-- STORAGE


type alias Storage =
    { odorikApi : OdorikApi.Model
    }



-- Converting to JSON


toJson : Storage -> Json.Value
toJson storage =
    Encode.object
        [ ( "odorikapi", OdorikApi.encoder storage.odorikApi )
        ]



-- Converting from JSON


fromJson : Json.Value -> Storage
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : Storage
init =
    { odorikApi = OdorikApi.init
    }

decoder : Json.Decoder Storage
decoder =
    Json.map Storage
        (Json.field "odorikapi" OdorikApi.decoder)

-- Updating storage


logout : Storage -> Cmd msg
logout storage =
    { storage | odorikApi = OdorikApi.logout storage.odorikApi }
        |> toJson
        |> save

login : Storage -> msg -> String -> String -> Cmd msg
login storage msg user pass =
    Cmd.batch
        [ { storage | odorikApi = OdorikApi.login storage.odorikApi user pass }
            |> toJson
            |> save
        , Task.succeed msg |> Task.perform identity ]

saveLine : Storage -> msg -> Maybe String -> Cmd msg
saveLine storage msg line =
    Cmd.batch
        [ { storage | odorikApi = OdorikApi.saveLine storage.odorikApi line }
            |> toJson
            |> save
        , Task.succeed msg |> Task.perform identity ]

saveCaller : Storage -> msg -> Maybe OdorikApi.SpeedDial -> Cmd msg
saveCaller storage msg caller =
    Cmd.batch
        [ { storage | odorikApi = OdorikApi.saveCaller storage.odorikApi caller }
            |> toJson
            |> save
        , Task.succeed msg |> Task.perform identity ]

saveSpeedDials : Storage -> msg -> List OdorikApi.SpeedDial -> Cmd msg
saveSpeedDials storage msg speedDials =
    Cmd.batch
        [ { storage | odorikApi = OdorikApi.saveSpeedDials storage.odorikApi speedDials }
            |> toJson
            |> save
        , Task.succeed msg |> Task.perform identity ]

-- LISTENING FOR STORAGE UPDATES


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)
