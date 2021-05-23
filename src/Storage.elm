port module Storage exposing
    ( Storage, fromJson, onChange
    , increment, decrement
    )

{-|

@docs Storage, fromJson, onChange
@docs increment, decrement

-}

import Json.Decode as Json
import Json.Encode as Encode
import Time



-- PORTS


port save : Json.Value -> Cmd msg


port load : (Json.Value -> msg) -> Sub msg



-- STORAGE


type alias Storage =
    { counter : Int
    , user : String
    , pass : String
    , line : Int
    , caller : String
    , balance : String
    , balanceTs : Time.Posix
    }



-- Converting to JSON


toJson : Storage -> Json.Value
toJson storage =
    Encode.object
        [ ( "counter", Encode.int storage.counter )
        , ( "user", Encode.string storage.user )
        , ( "pass", Encode.string storage.pass )
        , ( "line", Encode.int storage.line )
        , ( "caller", Encode.string storage.caller )
        , ( "balance", Encode.string storage.balance )
        , ( "balanceTs", Encode.int (Time.posixToMillis storage.balanceTs) )
        ]



-- Converting from JSON


fromJson : Json.Value -> Storage
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : Storage
init =
    { counter = 0
    , user = "888"
    , pass = "888"
    , line = 111
    , caller = "+420 800 123 456"
    , balance = "0.00"
    , balanceTs = (Time.millisToPosix 0)
    }

decodeTime : Json.Decoder Time.Posix
decodeTime =
    Json.int
        |> Json.andThen
            (\ms -> Json.succeed <| Time.millisToPosix ms )


decoder : Json.Decoder Storage
decoder =
    Json.map7 Storage
        (Json.field "counter" Json.int)
        (Json.field "user" Json.string)
        (Json.field "pass" Json.string)
        (Json.field "line" Json.int)
        (Json.field "caller" Json.string)
        (Json.field "balance" Json.string)
        (Json.field "balanceTs" decodeTime)

-- Updating storage


increment : Storage -> Cmd msg
increment storage =
    { storage | counter = storage.counter + 1 }
        |> toJson
        |> save


decrement : Storage -> Cmd msg
decrement storage =
    { storage | counter = storage.counter - 1 }
        |> toJson
        |> save



-- LISTENING FOR STORAGE UPDATES


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)
