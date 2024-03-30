module View exposing (..)

import Browser
import Dict
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import Model exposing (AppData, Dimensions, KeyPress(..), Model(..), Msg(..), unwrapModel)
import Texts.All exposing (texts)
import Theme exposing (theme)
import Translations.English as UserText
import Update exposing (changeListener)


view : Model -> Browser.Document Msg
view model =
    { title = "Infinitype"
    , body =
        [ El.layout
            [ Font.family [ Font.typeface "infinitype-mono", Font.monospace ]
            , Font.color theme.fontColor
            , Font.size <| round theme.textSize
            , Background.color theme.bgColor
            , El.htmlAttribute <| onClick GrabFocus
            ]
            (case (unwrapModel model).screen of
                Nothing ->
                    El.none

                Just screen ->
                    renderStates model screen
            )
        ]
    }


renderStates : Model -> Dimensions -> Element Msg
renderStates model screen =
    let
        bright =
            case model of
                Typing _ ->
                    True

                Paused _ ->
                    False

                _ ->
                    True

        appData =
            unwrapModel model

        typingLine =
            renderTypingArea
                model
                screen
                bright

        baseAttrs =
            [ El.centerY
            , El.centerX
            , El.width <| El.px (screen.width - 75)
            ]

        stateAttrs =
            case model of
                Paused _ ->
                    [ El.behindContent
                        (el
                            [ El.centerX
                            , El.centerY
                            , Font.size 200
                            ]
                            (El.text "||")
                        )
                    ]

                _ ->
                    []

        attrs =
            List.concat
                [ baseAttrs
                , stateAttrs
                ]

        typingArea =
            El.column attrs
                [ renderStats appData screen bright
                , typingLine
                , renderComposingHelp appData
                , renderTypingHelp appData screen
                ]

        width =
            screen.width - 50

        height =
            screen.height

        topCorners =
            El.row [ El.alignBottom ] []

        bottomCorners =
            El.row [ El.alignBottom ] []

        renderState =
            case model of
                CommandPalette _ ->
                    renderCommandPalette model screen

                _ ->
                    typingArea
    in
    el [] <|
        El.column
            [ El.height <| El.px height ]
            [ el
                [ El.centerX
                , El.width <| El.minimum width <| El.maximum width <| El.fill
                , El.height <| El.fillPortion 1
                ]
                topCorners
            , el [ El.height <| El.fillPortion 6 ] (el [ El.centerY ] renderState)
            , el [ El.height <| El.fillPortion 1 ] bottomCorners
            ]


id : String -> El.Attribute msg
id =
    Attr.id >> El.htmlAttribute


customEvent : String -> msg -> El.Attribute msg
customEvent evt message =
    El.htmlAttribute <| Html.Events.on evt (D.succeed message)


{-| Render an input field to capture the user's typing input.
Note that this input needs focus at all times while typing.

To make that happen, we use the `refocus` command.
This happens when the app is initialized, and at other points where focus is disturbed like unpausing.

-}
renderCursor : Bool -> AppData -> Element Msg
renderCursor bright appData =
    El.row [ El.centerX, El.centerY ]
        [ El.el
            [ Background.color theme.cursor
            , El.width <| El.px 2
            , El.height <| El.px (round theme.textSize)
            , El.alpha
                (if bright then
                    1

                 else
                    0.5
                )
            ]
            El.none
        , Input.text
            [ Input.focusedOnLoad
            , id "infinitype"
            , El.htmlAttribute <| Attr.tabindex 0
            , El.width <| El.px 1
            , El.height <| El.px (round theme.textSize)
            , El.alpha 0

            -- NOTE: This is to enable fancy language input methods like japanese
            , customEvent "compositionstart" (ComposingInput True)
            , customEvent "compositionend" (ComposingInput False)
            ]
            { text = appData.inputValue ++ appData.rawText
            , label = Input.labelHidden ""
            , onChange = changeListener
            , placeholder = Nothing
            }
        ]


renderLetter : KeyPress -> Bool -> AppData -> Element msg
renderLetter keyResult bright appData =
    let
        translateSpaces c =
            if c == ' ' then
                '_'

            else
                c

        mistakeHint actual =
            el
                [ El.centerX
                , Font.color theme.incorrectHintColor
                , Font.size <| round theme.textSize // 2
                ]
                (El.text <|
                    String.map
                        translateSpaces
                        actual
                )

        dimmableText color =
            if bright then
                color

            else
                theme.veryDim
    in
    case keyResult of
        Correct key _ ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.typedFontColor
                ]
            <|
                El.text key

        Untyped key ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.fontColor
                ]
            <|
                El.text key

        Incorrect actual intended _ ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.incorrect
                , El.below <| mistakeHint actual
                ]
            <|
                El.text <|
                    String.map translateSpaces intended


renderLetters : AppData -> Bool -> List KeyPress -> List (Element msg)
renderLetters appData bright words =
    words
        |> List.map (\l -> renderLetter l bright appData)


renderTypingArea : Model -> Dimensions -> Bool -> Element Msg
renderTypingArea model screen bright =
    let
        appData =
            unwrapModel model

        colWidth =
            screen.width // 2

        widthAttr =
            El.fill |> El.minimum colWidth |> El.maximum colWidth

        themeMonosize =
            theme.textSize * appData.corpusData.monosize

        charCount =
            floor <| ((toFloat screen.width / 2.0) / themeMonosize)

        recentlyTyped =
            appData.typed
                |> List.reverse
                |> List.take charCount
                |> List.reverse

        renderAppLetters =
            renderLetters appData bright

        leftColumn =
            el
                [ El.width widthAttr
                , El.alpha
                    (if bright then
                        1

                     else
                        0.2
                    )
                ]
                (El.row [ El.alignRight ]
                    (renderAppLetters recentlyTyped)
                )

        rightColumn =
            El.row
                [ El.width widthAttr ]
                (renderAppLetters (List.take charCount appData.typing))
    in
    El.row
        [ El.centerX, El.centerY ]
        [ leftColumn
        , renderCursor bright appData
        , rightColumn
        ]


renderStat : ( String, String ) -> Bool -> Element Msg
renderStat ( statName, value ) bright =
    let
        size =
            round theme.textSize // 2

        color =
            theme.typedFontColor

        primaryColor =
            if bright then
                theme.fontColor

            else
                theme.veryDim

        statsSize =
            floor (1.5 * theme.textSize)
    in
    El.column []
        [ el [ Font.size statsSize, Font.color primaryColor ] (El.text value)
        , el
            [ Font.size size
            , Font.color color
            , El.centerX
            ]
            (El.text statName)
        ]


renderStats : AppData -> Dimensions -> Bool -> Element Msg
renderStats appData screen bright =
    let
        adjustment =
            (toFloat screen.height / 4) - theme.textSize
    in
    el [ El.centerX, El.moveUp adjustment ]
        (El.column []
            [ renderStat ( "wpm", appData.stats.wpm ) bright
            ]
        )


renderPauseHelp : AppData -> Dimensions -> Element Msg
renderPauseHelp appData screens =
    case appData.screen of
        Nothing ->
            El.none

        Just screen ->
            el
                [ El.centerX
                , Font.size <| round theme.textSize // 2
                , El.moveDown <| (toFloat screen.height / 4)
                ]
                (El.column
                    []
                    [ El.text UserText.pauseHint ]
                )


renderTypingHelp : AppData -> Dimensions -> Element Msg
renderTypingHelp appData screen =
    let
        adjustment =
            toFloat screen.height / 5

        hint ( key, value ) =
            El.row [ El.width <| El.px 120 ]
                [ el [ El.width <| El.fillPortion 2 ] (El.text key)
                , el [ El.width <| El.fillPortion 1 ] (El.text value)
                ]
    in
    El.column
        [ El.centerX
        , Font.size <| round theme.textSize // 2
        , Font.color theme.veryDim
        , El.moveDown <| adjustment
        ]
        [ El.column []
            [ hint ( "pause", "⏎" )
            , hint ( "menu ", "␛" )
            , hint ( "reset", "⇥" )
            ]
        ]


renderCommandPalette : Model -> Dimensions -> Element Msg
renderCommandPalette model screen =
    let
        data =
            unwrapModel model

        color name =
            if data.corpusData.name == name then
                theme.fontColor

            else
                theme.veryDim

        itemsList =
            Dict.toList
                texts

        selectItem ( name, _ ) =
            el
                [ El.centerX
                , Font.color (color name)
                , Font.size 40
                ]
                (El.text name)
    in
    El.column [ El.width <| El.px <| screen.width ] <|
        List.map
            selectItem
            itemsList


renderComposingHelp : AppData -> Element msg
renderComposingHelp appData =
    let
        help =
            if appData.composingInput && String.length appData.rawText > 0 then
                appData.rawText

            else
                " "

        composingHelp =
            El.row [ El.alignRight, El.centerX, El.moveDown 50 ] [ El.text help ]
    in
    composingHelp
