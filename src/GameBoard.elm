module GameBoard exposing (..)

import Browser
import Flags exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- TODO:
-- being able to select number of players (e.g routing)


type Msg
    = ShowAnswer
    | CorrectAnswer
    | WrongAnswer
    | NewGame


type View
    = Question
    | Answer
    | Results


type alias Player =
    { name : String
    , score : Int
    }


type alias Model =
    { players : List Player
    , currentPlayerIndex : Int
    , flags : List Flag
    , currentFlagIndex : Int
    , view : View
    }


init : ( Model, Cmd Msg )
init =
    let
        players =
            [ Player "😋 Joey" 0
            , Player "u1F920 Ross" 0
            , Player "👩 Rachel" 0
            , Player "💆 Monica" 0
            , Player "🙆 Phoebe" 0
            ]

        flags =
            List.take 10 Flags.flags
    in
    ( Model players 0 flags 0 Question, Cmd.none )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, cmd ) =
    case msg of
        NewGame ->
            init

        ShowAnswer ->
            ( { model | view = Answer }, Cmd.none )

        CorrectAnswer ->
            case List.Extra.getAt model.currentPlayerIndex model.players of
                Just player ->
                    let
                        updatedPlayers =
                            model.players
                                |> List.Extra.updateAt model.currentPlayerIndex (\p -> { p | score = p.score + 1 })

                        nextPlayerIndex =
                            if (model.currentPlayerIndex + 1) >= List.length model.players then
                                0
                            else
                                model.currentPlayerIndex + 1

                        nextFlagIndex =
                            if (model.currentFlagIndex + 1) >= List.length model.flags then
                                0
                            else
                                model.currentFlagIndex + 1

                        nextView =
                            if (model.currentFlagIndex + 1) >= List.length model.flags then
                                Results
                            else
                                Question
                    in
                    ( { model
                        | players = updatedPlayers
                        , currentPlayerIndex = nextPlayerIndex
                        , currentFlagIndex = nextFlagIndex
                        , view = nextView
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        WrongAnswer ->
            case List.Extra.getAt model.currentPlayerIndex model.players of
                Just player ->
                    let
                        nextPlayerIndex =
                            if (model.currentPlayerIndex + 1) >= List.length model.players then
                                0
                            else
                                model.currentPlayerIndex + 1

                        nextFlagIndex =
                            if (model.currentFlagIndex + 1) >= List.length model.flags then
                                0
                            else
                                model.currentFlagIndex + 1

                        nextView =
                            if (model.currentFlagIndex + 1) >= List.length model.flags then
                                Results
                            else
                                Question
                    in
                    ( { model
                        | currentPlayerIndex = nextPlayerIndex
                        , currentFlagIndex = nextFlagIndex
                        , view = nextView
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


view : ( Model, Cmd Msg ) -> Html Msg
view ( model, cmd ) =
    let
        currentFlag =
            List.Extra.getAt model.currentFlagIndex model.flags

        boardView =
            case ( model.view, currentFlag ) of
                ( Answer, Just flag ) ->
                    div []
                        [ h1 [ class "h1" ] [ text "Guess the flag" ]
                        , h2 [ class "h2" ] [ text flag.countryName ]
                        , img [ class "game-board-img", src flag.imgUrl, alt "Flag" ] []
                        , button [ type_ "button", class "button", onClick CorrectAnswer ] [ text "Correct" ]
                        , button [ type_ "button", class "button", onClick WrongAnswer ] [ text "Wrong" ]
                        ]

                ( Question, Just flag ) ->
                    div []
                        [ h1 [ class "h1" ] [ text "Guess the flag" ]
                        , h2 [ class "h2" ] [ text "........" ]
                        , img [ class "game-board-img", src flag.imgUrl, alt "Flag" ] []
                        , button [ type_ "button", class "button", onClick ShowAnswer ] [ text "Reveal" ]
                        ]

                ( Results, _ ) ->
                    div []
                        [ h1 [ class "h1" ] [ text "And the winner is..." ]
                        , case List.head (List.reverse (List.sortBy .score model.players)) of
                            Just player ->
                                h2 [ class "h2" ] [ text player.name ]

                            Nothing ->
                                h2 [ class "h2" ] [ text "Oops, no winner!" ]
                        , button [ class "button", onClick NewGame ] [ text "New game" ]
                        ]

                ( _, Nothing ) ->
                    div [] []
    in
    div []
        [ div [ class "game-board-players" ]
            (List.indexedMap (\i p -> viewPlayer i ( model.currentPlayerIndex, p )) model.players)
        , viewProgressBar model
        , boardView
        ]


viewProgressBar model =
    let
        width =
            String.fromInt ((100 // List.length model.flags) * model.currentFlagIndex)
    in
    div [ class "progress-bar" ]
        [ span [ class "progress-bar__status" ]
            [ text (String.fromInt model.currentFlagIndex ++ " / " ++ String.fromInt (List.length model.flags)) ]
        , div
            [ class "progress-bar__bar"
            , style "width" <| width ++ "%"
            ]
            []
        ]


viewPlayer : Int -> ( Int, Player ) -> Html msg
viewPlayer index ( currentPlayerIndex, player ) =
    div [ classList [ ( "game-board-player", True ), ( "game-board-player__active", index == currentPlayerIndex ) ] ]
        [ span [] [ text (player.name ++ " ") ]
        , span [] [ text (String.fromInt player.score) ]
        ]
