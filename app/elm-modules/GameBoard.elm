port module GameBoard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Flags exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- TODO:
-- results view
-- being able to select number of players
-- styles

type Msg =
    ShowAnswer
    | CorrectAnswer
    | WrongAnswer


type View
    = Question
    | Answer


type alias Player =
    { name : String
    , score : Int
    }


type alias Model =
    { players : List Player
    , currentPlayerIndex : Int
    , flags: List Flag
    , currentFlagIndex: Int
    , view : View
    }


init : ( Model, Cmd Msg )
init =
    let
        players =
            [ Player "😋 Joey" 0
            , Player "🤠 Ross" 0
            , Player "👩 Rachel" 0
            , Player "💆 Monica" 0
            , Player "🙆 Phoebe's" 0
            ]
    in
        ( Model players 0 flags 0 Question, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowAnswer ->
            ( { model | view = Answer }, Cmd.none )

        CorrectAnswer ->
            case List.Extra.getAt model.currentPlayerIndex model.players of
                Just player ->
                    let
                        updatedPlayers =
                            Maybe.withDefault model.players
                                (List.Extra.updateAt
                                    model.currentPlayerIndex
                                    (\p -> { p | score = p.score + 1 })
                                    model.players
                                )

                        nextPlayerIndex =
                            if (model.currentPlayerIndex + 1) >= List.length model.players then
                                0
                            else
                                model.currentPlayerIndex + 1

                        nextFlagIndex =
                            if (model.currentFlagIndex + 1) >= List.length model.flags then
                              -- TODO: show results
                              0
                            else
                                model.currentFlagIndex + 1
                    in
                        ( { model
                            | players = updatedPlayers
                            , currentPlayerIndex = nextPlayerIndex
                            , currentFlagIndex = nextFlagIndex
                            , view = Question
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
                              -- TODO: show results
                              0
                            else
                                model.currentFlagIndex + 1
                    in
                        ( { model
                            | currentPlayerIndex = nextPlayerIndex
                            , currentFlagIndex = nextFlagIndex
                            , view = Question
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

view : Model -> Html Msg
view model =
    let
        currentFlag =
          List.Extra.getAt model.currentFlagIndex model.flags
        boardView =
            case (model.view, currentFlag) of
                (Answer, Just flag) ->
                    div []
                        [ h1 [ class "h1" ] [ text "Guess the flag" ]
                        , h2 [ class "h2" ] [ text flag.countryName ]
                        , img [ class "game-board-img", src flag.imgUrl, alt "Flag" ] []
                        , button [ type_ "button", class "button", onClick CorrectAnswer ] [ text "Correct" ]
                        , button [ type_ "button", class "button", onClick WrongAnswer ] [ text "Wrong" ]
                        ]

                (Question, Just flag) ->
                    div []
                        [ h1 [ class "h1" ] [ text "Guess the flag" ]
                        , h2 [ class "h2" ] [ text "........" ]
                        , img [ class "game-board-img", src flag.imgUrl, alt "Flag" ] []
                        , button [ type_ "button", class "button", onClick ShowAnswer ] [ text "Reveal" ]
                        ]

                _ ->
                  div [] []
    in
        div []
            [ div [ class "game-board-players" ]
                (List.indexedMap (\i p -> viewPlayer i (model.currentPlayerIndex, p)) model.players)
            , viewProgressBar model
            , boardView
            ]

viewProgressBar model =
   div [ class "progress-bar" ]
       [ span [ class "progress-bar__status" ]
           [ text ((toString model.currentFlagIndex) ++ " / " ++ (toString (List.length model.flags))) ]
       , div [ class "progress-bar__bar"
             , style [("width",  (toString ((100 / (toFloat (List.length model.flags))) * toFloat model.currentFlagIndex)) ++ "%") ]
             ]
             []
       ]

viewPlayer : Int -> (Int, Player) -> Html msg
viewPlayer index (currentPlayerIndex, player) =
    div [ classList [ ( "game-board-player", True ), ( "game-board-player__active", index == currentPlayerIndex ) ] ]
        [ span [] [ text (player.name ++ " ") ]
        , span [] [ text (toString player.score) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
