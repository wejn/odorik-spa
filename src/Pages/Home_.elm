module Pages.Home_ exposing (page)

import Html
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)

page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view req
        }

view : Request -> View msg
view req =
    { title = "Homepage"
    , body = [ Html.text ("Hello, " ++ req.url.host ++ "!") ]
    }
