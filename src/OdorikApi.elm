module OdorikApi exposing
    ( Model
    , ApiResponse
    , init
    , decoder
    , encoder
    , apiUrlFromCreds
    , getBalance
    , errorToString
    )

import Http
import Json.Decode as Json
import Json.Encode as Encode
import Task
import Time
import Url.Builder as UB

type alias Model =
    { user : String
    , pass : String
    , line : Int
    , caller : String
    }

type alias ApiResponse =
    Result Http.Error String

init : Model
init =
    { user = "888"
    , pass = "888"
    , line = 111
    , caller = "+420 800 123 456"
    }

decoder : Json.Decoder Model
decoder =
    Json.map4 Model
        (Json.field "user" Json.string)
        (Json.field "pass" Json.string)
        (Json.field "line" Json.int)
        (Json.field "caller" Json.string)

encoder : Model -> Encode.Value
encoder m =
    Encode.object
        [ ("user", Encode.string m.user)
        , ("pass", Encode.string m.pass)
        , ("line", Encode.int m.line)
        , ("caller", Encode.string m.caller)
        ]

serverFromCreds : Model -> String
serverFromCreds m =
    case (m.user, m.pass) of
        ("888", "888") -> "https://wejn.com/odorik_test_api___"
        _ -> "https://www.odorik.cz/api/v1"

apiUrlFromCreds : Model -> String -> String
apiUrlFromCreds m api =
    UB.custom (UB.CrossOrigin (serverFromCreds m)) [api] [UB.string "user" m.user, UB.string "password" m.pass] Nothing

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl _ ->
            "invalid url"
        Http.Timeout ->
            "timeout"
        Http.NetworkError ->
            "network error"
        Http.BadStatus 500 ->
            "server error"
        Http.BadStatus 400 ->
            "call error"
        Http.BadStatus code ->
            "unknown error: " ++ (String.fromInt code)
        Http.BadBody errorMessage ->
            errorMessage

expectApiResponse : (Result Http.Error String -> msg) -> Http.Expect msg
expectApiResponse toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case String.startsWith "error " body of
                        True -> Err (Http.BadBody ("api: " ++ (String.dropLeft 6 (String.trim body))))
                        _ -> Ok body

haveValidCredentials : Model -> Bool
haveValidCredentials m =
    not ((String.isEmpty m.user) || (String.isEmpty m.pass))

getBalance : Model -> (ApiResponse -> msg) -> Cmd msg
getBalance model msg =
    case haveValidCredentials model of
        False ->
            Cmd.batch [Task.perform (\_ -> msg (Err (Http.BadBody "not logged in"))) Time.now]
        True ->
            Http.get
                { url = apiUrlFromCreds model "balance" -- FIXME: mod the time, to force refresh
                , expect = expectApiResponse msg }
