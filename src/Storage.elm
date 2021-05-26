port module Storage exposing
    ( Storage, fromJson, onChange
    , logout
    , login
    )

{-|

@docs Storage, fromJson, onChange
@docs logout, login

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

-- LISTENING FOR STORAGE UPDATES


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)
