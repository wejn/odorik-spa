module Attr exposing (..)

import Dropdown
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

linkLikeButton : List (Attribute msg)
linkLikeButton =
    [ Border.width 0
    , Border.rounded 0
    ] ++ link

error : List (Attribute msg)
error =
    [ Font.color <| rgb255 204 0 0 ]

input : List (Attribute msg)
input =
    [ Border.width 1, Border.rounded 3, Border.color <| rgb255 136 138 133, padding 3 ]

link : List (Attribute msg)
link =
    [ Font.underline, Font.color <| rgb255 0 0 128 ]

dropdownAugment : Dropdown.Config item msg model -> Dropdown.Config item msg model
dropdownAugment c =
    let
        containerAttrs =
            [ width fill ]

        selectAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, spacing 10, width fill ]

        searchAttrs =
            [ Border.width 0, padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 0
            ]
    in
    c
    |> Dropdown.withContainerAttributes containerAttrs
    |> Dropdown.withSelectAttributes selectAttrs
    |> Dropdown.withListAttributes listAttrs
    |> Dropdown.withSearchAttributes searchAttrs

dropdownBackgroundColor : Bool -> Bool -> Color
dropdownBackgroundColor selected highlighted =
    if selected then
        rgb255 100 100 100

    else if highlighted then
        rgb255 250 250 250

    else
        rgb255 255 255 255

