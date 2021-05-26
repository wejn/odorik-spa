module Attr exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

button : List (Attribute msg)
button = 
    [ padding 10
    , Border.width 2
    , Border.rounded 16
    , Border.color <| rgb255 0x50 0x50 0x50
    , Background.color <| rgb255 0xbb 0xdd 0xff
    , Font.color <| rgb255 255 255 255
    ]

error : List (Attribute msg)
error =
    [ Font.color <| rgb255 204 0 0 ]

greenButton : List (Attribute msg)
greenButton =
    [ Background.color <| rgb255 115 210 22
    , Border.color <| rgb255 78 154 6
    , Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , Font.color <| rgb255 255 255 255
    , paddingXY 20 6
    ]

input : List (Attribute msg)
input =
    [ Border.width 1, Border.rounded 3, Border.color <| rgb255 136 138 133, padding 3 ]
