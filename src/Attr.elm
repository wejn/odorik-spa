module Attr exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

button : List (Attribute msg)
button =
    [ paddingXY 10 6
    , Border.width 2
    , Border.rounded 3
    , Border.color <| rgb255 0x50 0x50 0x50
    , Background.color <| rgb255 0xbb 0xdd 0xff
    , Font.color <| rgb255 255 255 255
    ]

error : List (Attribute msg)
error =
    [ Font.color <| rgb255 204 0 0 ]

input : List (Attribute msg)
input =
    [ Border.width 1, Border.rounded 3, Border.color <| rgb255 136 138 133, padding 3 ]
