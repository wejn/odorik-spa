module Pages.Home_ exposing (Model, Msg, init, page, update, view)

import Attr
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Gen.Route as Route
import Html
import Html.Events
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req
        , update = update shared.storage
        , view = view req shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
    }


init : Request -> ( Model, Cmd Msg )
init req =
    ( { } , Cmd.none )



-- UPDATE


type Msg
    = None


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        None -> ( model , Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

view : Request -> Shared.Model -> Model -> View Msg
view req shared m =
    let
        odorikApi = "https://www.odorik.cz/w/api"
        blogPost = "https://wejn.org/2021/06/making-ios-custom-dialer-app-with-shortcuts-and-elm-spa/"
        shortcut = "https://www.icloud.com/shortcuts/676a2e2fdb2b40e2ba5c1c182238d54d"
        source = "https://github.com/wejn/odorik-spa"
        monospaced = (\x -> el [ Font.family [ Font.monospace ], Background.color <| rgb255 242 242 242 ] x)
        italic = (\x -> el [ Font.italic ] x )
        heading = (\x -> paragraph [ Font.size 24, Font.bold ] [ text x ])
    in
    Shared.view shared req "Home" <|
        column [ width fill, height fill, spacing 20 ]
            [ heading "Introduction"
            , paragraph [ Font.justify ]
                [ text "This is a single-page app for initiating callback using "
                , newTabLink Attr.link { label = text "Odorik.cz API", url = odorikApi }
                , text ", and primarily focused on a reasonable integration with iOS."
                ]
            , paragraph [ Font.justify ]
                [ text "The main reason for its existence is that I could no longer justify keeping the "
                , newTabLink Attr.link { label = text "Odorik iOS App", url = "https://wejn.com/ios/odorik/" }
                , text " alive (the Apple Developer program is rather expensive), and without that app, "
                , text " it is rather difficult to initiate callbacks."
                ]
            , paragraph [ Font.justify ]
                [ text "You can read much more background information about this app "
                , newTabLink Attr.link { label = text "on my blog", url = blogPost }
                , text " (if that strikes you as interesting)."
                ]
            , paragraph [ Font.justify ]
                [ text "And while you can use the app without the iOS "
                , newTabLink Attr.link { label = text "contact selection", url = shortcut }
                , text " Shortcut (by simply utilizing the Speed Dials), I think you'd be so much worse off without it."
                ]
            , heading "Installation"
            , paragraph [ Font.justify ]
                [ text "1. Login (on your mobile device) "
                , link Attr.link { label = text "on the Settings page", url = (Route.toHref Route.Settings) }
                , text ", using your odorik.cz credentials."
                ]
            , paragraph [ Font.justify ]
                [ text "2. Enable third-party Shortcuts on your phone "
                , italic <| text "(System settings → Shortcuts → Allow Untrusted Shortcuts)"
                , text ", and download the "
                , newTabLink Attr.link { label = text "contact selection", url = shortcut }
                , text " Shortcut (see the "
                , newTabLink Attr.link { label = text "blog post", url = blogPost }
                , text " for more info)."
                ]
            , paragraph [ Font.justify ]
                [ text "3. Add the shortcut to your Home screen (for quick access), by tapping the three dots"
                , text "repeatedly until you get "
                , italic <| text "\"Add to Home screen\""
                , text "."
                ]
            , paragraph [ Font.justify ]
                [ text "4. Use the home screen icon to setup callbacks. (And you can use the site to check balance)"
                ]
            , heading "Demo credentials"
            , paragraph [ Font.justify ]
                [ text "Because it is inevitable that someone randomly bumping into this app doesn't have "
                , text "\"live\" credentials, but would like to see this app in action, I have hardcoded "
                , text "demo credentials that will force the app to use a testing api endpoint (under my control)."
                ]
            , paragraph [ Font.justify ]
                [ text "Simply use \""
                , el [ Font.bold ] <| text "888"
                , text "\" for both login and password."
                ]
            , heading "Privacy statement"
            , paragraph [ Font.justify ]
                [ text "This app deliberately doesn't use cookies, and doesn't track you in any way."
                ]
            , paragraph [ Font.justify ]
                [ text "In fact, it is purposefully designed to keep all your data yours, by only using "
                , monospaced <| text "localStorage"
                , text " and being written as an SPA (Single Page App)."
                ]
            , paragraph [ Font.justify ]
                [ text "By using "
                , monospaced <| text "localStorage"
                , text " the only thing I could ever see in server logs is the URL "
                , text "that your browser requests. And because it's an SPA, I only see the initial page load. "
                , text "Isn't that neat, in this "
                , italic <| text "we will track you to death"
                , text " day and age?"
                ]
            , paragraph [ Font.justify ]
                [ text "Don't take my word for it, though. Check the "
                , newTabLink Attr.link { label = text "source code", url = source }
                , text " for yourself, if you so please."
                ]
            , heading "Credit"
            , paragraph [ Font.justify ]
                [ text "© 2021 "
                , newTabLink Attr.link { label = text "Michal Jirků", url = "https://wejn.org/" }
                , text "."
                ]
        ]
