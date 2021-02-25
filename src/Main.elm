module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as EK
import Html.Attributes as Attr
import Json.Decode as D
import Process
import Random
import Task
import Time
import Url exposing (Url)


type Model
    = NotStarted
    | Circle (Maybe Command) Level
    | Change Command Level


type Level
    = Upper
    | Middle
    | Lower


type Command
    = Up
    | Down
    | ChangePlane
    | ChangeDirection


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type alias Flags =
    {}


init : D.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( NotStarted, Cmd.none )


newCommand : Level -> Maybe Command -> Cmd Msg
newCommand level maybePreviousCommand =
    let
        possibleCommands =
            case level of
                Upper ->
                    upperCommands

                Middle ->
                    middleCommands

                Lower ->
                    lowerCommands
    in
    Random.generate NewCommand <|
        case List.filter (\c -> Just c /= maybePreviousCommand) possibleCommands of
            [] ->
                allCommandsGenerator

            first :: rest ->
                Random.uniform first rest


allCommandsGenerator : Random.Generator Command
allCommandsGenerator =
    Random.uniform Up [ Down, ChangePlane, ChangeDirection ]


upperCommands : List Command
upperCommands =
    [ Down, Down, ChangePlane, ChangeDirection ]


middleCommands : List Command
middleCommands =
    [ Up, Down, ChangePlane, ChangeDirection ]


lowerCommands : List Command
lowerCommands =
    [ Up, Up, ChangePlane, ChangeDirection ]


view : Model -> Browser.Document Msg
view model =
    { title = "Circles Game"
    , body =
        [ E.layout
            [ Background.color backgroundColor
            , Font.color foregroundColor
            , Font.size 60
            , Font.family
                [ Font.typeface "Patrick Hand"
                , Font.sansSerif
                ]
            , E.height E.fill
            , E.width E.fill
            ]
            (case model of
                NotStarted ->
                    Input.button
                        [ E.centerX
                        , E.centerY
                        , E.padding 10
                        , Background.color foregroundColor
                        , Font.color backgroundColor
                        , Border.color foregroundColor
                        , Border.width 1
                        , Border.shadow { blur = 5, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
                        , Border.rounded 8
                        ]
                        { onPress = Just Start, label = E.text "Start" }

                Change command _ ->
                    gameLayout (commandToColor command) <|
                        E.el
                            [ E.centerX, E.centerY ]
                            (E.text (commandToString command))

                Circle previousCommand level ->
                    case previousCommand of
                        Just command ->
                            gameLayout backgroundColor <|
                                E.column []
                                    [ E.el
                                        [ E.centerX, E.centerY ]
                                        (E.text (commandToString command))
                                    , E.el
                                        [ E.centerX, E.centerY ]
                                        (E.text (levelToString level ++ " level"))
                                    ]

                        Nothing ->
                            gameLayout backgroundColor <|
                                E.el
                                    [ E.centerX, E.centerY ]
                                    (E.text (levelToString level ++ " level"))
            )
        ]
    }


gameLayout color content =
    E.column
        [ E.height E.fill
        , E.width E.fill
        , E.padding 20
        , Background.color color
        ]
        [ E.row
            [ Font.glow backgroundColor 4
            , E.height E.fill
            , E.centerX
            ]
            [ content ]
        , E.row [ E.centerX ]
            [ Input.button
                [ E.centerX
                , E.centerY
                , E.padding 10
                , Background.color foregroundColor
                , Font.color backgroundColor
                , Border.color foregroundColor
                , Border.width 1
                , Border.shadow { blur = 5, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
                , Border.rounded 8
                ]
                { onPress = Just Stop, label = E.text "Stop" }
            ]
        ]


commandToColor : Command -> E.Color
commandToColor command =
    case command of
        Up ->
            E.rgb255 42 157 143

        Down ->
            E.rgb255 233 196 106

        ChangePlane ->
            E.rgb255 244 162 97

        ChangeDirection ->
            E.rgb255 231 111 81


commandToString : Command -> String
commandToString command =
    case command of
        Up ->
            "Up"

        Down ->
            "Down"

        ChangePlane ->
            "Change Plane"

        ChangeDirection ->
            "Change Direction"


levelToString : Level -> String
levelToString level =
    case level of
        Upper ->
            "Upper"

        Middle ->
            "Middle"

        Lower ->
            "Lower"


type Msg
    = NoOp
    | Start
    | Stop
    | NewCommand Command
    | NextCommand
    | ShowCircle Command Level
    | Sleep Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Start ->
            ( Circle Nothing Upper
            , Random.generate Sleep (Random.int 4 8)
            )

        Stop ->
            ( NotStarted, Cmd.none )

        NextCommand ->
            ( model
            , case model of
                Change previousCommand level ->
                    newCommand level (Just previousCommand)

                NotStarted ->
                    Cmd.none

                Circle previousCommand level ->
                    newCommand level previousCommand
            )

        NewCommand command ->
            let
                newLevel =
                    case model of
                        NotStarted ->
                            Upper

                        Change _ level ->
                            levelFromCommand level command

                        Circle _ level ->
                            levelFromCommand level command
            in
            ( Change command newLevel
            , Task.perform (\_ -> ShowCircle command newLevel) (Process.sleep 2000)
            )

        ShowCircle command level ->
            ( Circle (Just command) level
            , Random.generate Sleep (Random.int 4 8)
            )

        Sleep time ->
            ( model
            , Task.perform (\_ -> NextCommand) (Process.sleep (toFloat time * 1000))
            )


levelFromCommand : Level -> Command -> Level
levelFromCommand level command =
    case ( level, command ) of
        ( Upper, Down ) ->
            Middle

        ( Middle, Up ) ->
            Upper

        ( Middle, Down ) ->
            Lower

        ( Lower, Up ) ->
            Middle

        _ ->
            level


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


foregroundColor : E.Color
foregroundColor =
    E.rgb255 38 70 83


backgroundColor : E.Color
backgroundColor =
    E.rgb 0.89 0.89 0.89