module MainView exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Messages exposing (Msg)
import Models exposing (MainModel)
import Views.ViewHelpers exposing (attrFloat, attrInt)


mainView : MainModel -> Html Msg
mainView model =
    let
        width =
            model.windowSize.width

        height =
            model.windowSize.height

        --minWindowWidth =
        --    playFieldWidth + rightColumnWidth + marginBetweenBlocks
        --
        --minWindowHeight =
        --    playFieldHeight
    in
    --if width < toFloat minWindowWidth || height < toFloat minWindowHeight then
    --    errorScreen
    --
    --else
    div
        [ attrFloat (Attr.style "width") width
        , attrFloat (Attr.style "height") height
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        ]
        [ div [] [ text "working" ]

        --[ attrInt (Attr.style "width") minWindowWidth
        --, attrInt (Attr.style "height") minWindowHeight
        --, Attr.style "display" "flex"
        --, Attr.style "justify-content" "space-between"
        --]
        --[ -- playing field
        --  div
        --    [ attrInt (Attr.style "width") playFieldWidth
        --    , Attr.style "display" "flex"
        --    , Attr.style "justify-content" "center"
        --    , Attr.style "align-items" "center"
        --    ]
        --    [ div
        --        [ Attr.style "width" "100%"
        --        , attrInt (Attr.style "height") playFieldHeight
        --        , Attr.style "background-color" "black"
        --        ]
        --        [ renderPlayField model ]
        --    ]
        --]
        ]
