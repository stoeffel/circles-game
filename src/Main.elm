port module Main exposing (..)

import Animator
import Animator.Inline
import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as EK
import Html
import Html.Attributes as Attr
import Json.Decode as D
import Process
import Random
import Task
import Time
import Url exposing (Url)


type alias Model =
    Animator.Timeline State


type State
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


port speak : String -> Cmd msg


type alias Flags =
    {}


init : D.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Animator.init NotStarted, Cmd.none )


newCommand : Level -> Cmd Msg
newCommand level =
    let
        ( first, rest ) =
            case level of
                Upper ->
                    upperCommands

                Middle ->
                    middleCommands

                Lower ->
                    lowerCommands
    in
    Random.generate NewCommand <|
        Random.weighted first rest


upperCommands : ( ( Float, Command ), List ( Float, Command ) )
upperCommands =
    ( ( 0.4, Down ), [ ( 0.3, ChangePlane ), ( 0.3, ChangeDirection ) ] )


middleCommands : ( ( Float, Command ), List ( Float, Command ) )
middleCommands =
    ( ( 0.3, Up ), [ ( 0.3, Down ), ( 0.2, ChangePlane ), ( 0.2, ChangeDirection ) ] )


lowerCommands : ( ( Float, Command ), List ( Float, Command ) )
lowerCommands =
    ( ( 0.4, Up ), [ ( 0.3, ChangePlane ), ( 0.3, ChangeDirection ) ] )


view : Model -> Browser.Document Msg
view model =
    { title = "Circles Game"
    , body =
        [ E.layout
            [ Background.color <|
                E.fromRgb <|
                    Color.toRgba <|
                        Animator.color model
                            (\state ->
                                Color.fromRgba <|
                                    E.toRgb <|
                                        case state of
                                            Change command _ ->
                                                commandToColor command

                                            _ ->
                                                backgroundColor
                            )
            , E.htmlAttribute <|
                Animator.Inline.opacity model
                    (\state ->
                        case state of
                            Circle (Just command) _ ->
                                Animator.at 0.7

                            _ ->
                                Animator.at 1
                    )
            , Font.color foregroundColor
            , Font.size 30
            , Font.family
                [ Font.typeface "Patrick Hand"
                , Font.sansSerif
                ]
            , E.height E.fill
            , E.width E.fill
            ]
            (case Animator.current model of
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
                    gameLayout
                        [ icon Big <| commandToIcon command
                        , E.text (commandToString command)
                        ]

                Circle previousCommand level ->
                    case previousCommand of
                        Just command ->
                            gameLayout
                                [ icon Big <| commandToIcon command
                                , E.text (levelToString level ++ " level")
                                ]

                        Nothing ->
                            gameLayout
                                [ icon Big "far fa-circle"
                                , E.text (levelToString level ++ " level")
                                ]
            )
        ]
    }


type IconSize
    = Big
    | Small


icon size icon_ =
    E.html <|
        Html.i
            [ Attr.style "font-size" <|
                case size of
                    Big ->
                        "4em"

                    Small ->
                        "0.5em"
            , Attr.style "text-align" "center"
            , Attr.class icon_
            ]
            []


gameLayout content =
    E.column
        [ E.height E.fill
        , E.width E.fill
        , E.padding 20
        ]
        (E.row [ E.centerX ]
            [ Input.button
                [ E.centerX
                , E.centerY
                , E.padding 20
                , E.focused []
                , Border.rounded 8
                ]
                { onPress = Just Stop, label = icon Small "fas fa-times" }
            ]
            :: List.indexedMap
                (\index c ->
                    E.row
                        [ Font.glow backgroundColor 4
                        , if index == 0 then
                            E.height E.fill

                          else
                            E.height (E.maximum 80 E.fill)
                        , E.centerX
                        ]
                        [ c ]
                )
                content
        )


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


commandToIcon : Command -> String
commandToIcon command =
    "fas "
        ++ (case command of
                Up ->
                    "fa-arrow-up"

                Down ->
                    "fa-arrow-down"

                ChangePlane ->
                    "fa-layer-group"

                ChangeDirection ->
                    "fa-directions"
           )


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
    | Tick Time.Posix
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
            ( Animator.go Animator.veryQuickly (Circle Nothing Upper) model
            , Random.generate Sleep (Random.int 3 7)
            )

        Stop ->
            ( Animator.init NotStarted, Navigation.reload )

        NextCommand ->
            ( model
            , case Animator.current model of
                Change previousCommand level ->
                    Cmd.none

                NotStarted ->
                    Cmd.none

                Circle previousCommand level ->
                    newCommand level
            )

        NewCommand command ->
            let
                newLevel =
                    case Animator.current model of
                        NotStarted ->
                            Upper

                        Change _ level ->
                            levelFromCommand level command

                        Circle _ level ->
                            levelFromCommand level command
            in
            ( Animator.go Animator.veryQuickly (Change command newLevel) model
            , Cmd.batch
                [ Task.perform (\_ -> ShowCircle command newLevel) (Process.sleep 2000)
                , speak (commandToString command)
                ]
            )

        ShowCircle command level ->
            ( Animator.go Animator.verySlowly (Circle (Just command) level) model
            , Random.generate Sleep (Random.int 3 7)
            )

        Sleep time ->
            ( model
            , Task.perform (\_ -> NextCommand) (Process.sleep (toFloat time * 1000))
            )

        Tick newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
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
    Animator.toSubscription Tick model animator


animator =
    Animator.animator
        |> Animator.watching
            identity
            (\timeline _ -> timeline)


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
