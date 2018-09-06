module GameBoard exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Flags


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- TODO:
-- being able to select number of players (e.g routing)


type alias Model =
    { screen : Screen
    }


type Screen
    = Setup Int Flags.Continent
    | Question GameModel
    | Answer GameModel
    | Error String
    | Results (List Player)


type alias GameModel =
    { otherPlayers : List Player
    , activePlayer : Player
    , remainingFlags : List Flags.Flag
    , totalFlags : Int
    }


type alias Player =
    { name : String
    , score : Int
    }


type Msg
    = UpdateSetup Int Flags.Continent
    | StartGame Int Flags.Continent
    | ShowAnswer GameModel
    | MarkAsGood GameModel
    | MarkAsBad GameModel


init : ( Model, Cmd Msg )
init =
    ( Model <| Setup 0 Flags.Europe, Cmd.none )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, cmd ) =
    case msg of
        UpdateSetup numberOfPlayers continent ->
            ( { model | screen = Setup numberOfPlayers continent }, cmd )

        StartGame numberOfPlayers continent ->
            let
                players =
                    List.take numberOfPlayers availablePlayers

                screen =
                    case players of
                        activePlayer :: otherPlayers ->
                            let
                                remainingFlags =
                                    Flags.getFlagsForContinent continent

                                totalFlags =
                                    List.length remainingFlags
                            in
                            Question
                                { otherPlayers = otherPlayers
                                , activePlayer = activePlayer
                                , remainingFlags = remainingFlags
                                , totalFlags = totalFlags
                                }

                        _ ->
                            Error "There is not enough players"
            in
            ( { screen = screen }, Cmd.none )

        ShowAnswer gameModel ->
            ( { model | screen = Answer gameModel }, Cmd.none )

        MarkAsGood ({ activePlayer, otherPlayers, remainingFlags } as gameModel) ->
            let
                updatedPlayer =
                    { activePlayer | score = activePlayer.score + 1 }

                newRemainingFlags =
                    List.drop 1 remainingFlags

                newScreen =
                    if List.length newRemainingFlags <= 0 then
                        let
                            allPlayers =
                                otherPlayers ++ [ activePlayer ]
                        in
                        Results allPlayers
                    else
                        case otherPlayers of
                            newActivePlayer :: newOtherPlayers ->
                                Question
                                    { gameModel
                                        | otherPlayers = newOtherPlayers ++ [ updatedPlayer ]
                                        , activePlayer = newActivePlayer
                                        , remainingFlags = newRemainingFlags
                                    }

                            _ ->
                                Error "There is not enough players"
            in
            ( { model | screen = newScreen }, Cmd.none )

        MarkAsBad ({ activePlayer, otherPlayers, remainingFlags } as gameModel) ->
            let
                newRemainingFlags =
                    List.drop 1 remainingFlags

                newScreen =
                    if List.length newRemainingFlags <= 0 then
                        Results (otherPlayers ++ [ activePlayer ])
                    else
                        case otherPlayers of
                            newActivePlayer :: newOtherPlayers ->
                                Question
                                    { gameModel
                                        | otherPlayers = newOtherPlayers ++ [ activePlayer ]
                                        , activePlayer = newActivePlayer
                                        , remainingFlags = List.drop 1 remainingFlags
                                    }

                            _ ->
                                Error "There is not enough players"
            in
            ( { model | screen = newScreen }, Cmd.none )


view : ( Model, Cmd Msg ) -> Html Msg
view ( model, cmd ) =
    let
        viewBoard =
            case model.screen of
                Setup numberOfPlayers continent ->
                    viewSetup numberOfPlayers continent

                Question gameModel ->
                    viewQuestion gameModel

                Answer gameModel ->
                    viewAnswer gameModel

                Error msg ->
                    viewError msg

                Results players ->
                    viewResults players
    in
    div []
        [ viewBoard
        , text <| Debug.toString model
        , node "link"
            [ href "https://cdnjs.cloudflare.com/ajax/libs/tachyons/4.11.1/tachyons.min.css"
            , rel "stylesheet"
            , type_ "text/css"
            ]
            []
        ]


viewSetup numberOfPlayers selectedContinent =
    let
        buttons =
            List.range 2 6
                |> List.map
                    (\index ->
                        button
                            [ onClick (UpdateSetup index selectedContinent)
                            , classList
                                [ ( "f6 link dim br2 ph3 pv2 mb2 dib white", True )
                                , ( "bg-dark-green", numberOfPlayers /= index )
                                , ( "bg-dark-blue", numberOfPlayers == index )
                                ]
                            ]
                            [ text <| String.fromInt index ]
                    )

        continentOptions =
            Flags.allContinents
                |> List.map
                    (\continent ->
                        option
                            [ value <| Flags.continentToString continent
                            , selected (continent == selectedContinent)
                            ]
                            [ text <| Flags.continentToString continent ]
                    )
    in
    div []
        [ h1 [] [ text "How many players?" ]
        , div [] buttons
        , select
            [ on "change" (Json.Decode.map (UpdateSetup numberOfPlayers) targetValueContinentDecoder) ]
            continentOptions
        , div []
            [ button
                [ onClick <| StartGame numberOfPlayers selectedContinent
                , disabled <| numberOfPlayers <= 0
                ]
                [ text "Start game" ]
            ]
        ]


viewQuestion gameModel =
    let
        -- TODO: think how can I get rid of this
        flagEmoji =
            case List.head gameModel.remainingFlags of
                Just flag ->
                    flag.emojiFlag

                Nothing ->
                    ""
    in
    div
        []
        [ viewStats gameModel
        , div [] [ text "......." ]
        , div []
            [ text flagEmoji
            ]
        , div []
            [ button
                [ onClick <| ShowAnswer gameModel
                ]
                [ text "Show answer" ]
            ]
        ]


viewStats gameModel =
    div [ classList [ ( "bg-yellow-light", True ) ] ] []


viewAnswer gameModel =
    let
        -- TODO: think how can I get rid of this
        ( emojiFlag, countryName ) =
            case List.head gameModel.remainingFlags of
                Just flag ->
                    ( flag.emojiFlag, flag.countryName )

                Nothing ->
                    ( "", "" )
    in
    div
        []
        [ viewStats gameModel
        , div [] [ text countryName ]
        , div []
            [ text emojiFlag
            ]
        , div []
            [ button
                [ onClick <| MarkAsGood gameModel
                ]
                [ text "Good" ]
            , button
                [ onClick <| MarkAsBad gameModel
                ]
                [ text "Bad" ]
            ]
        ]


viewError msg =
    div [] [ text <| "There was an error: " ++ msg ]


viewResults players =
    div [] [ text <| Debug.toString players ]



-- HELPER FUNCITONS


availablePlayers =
    [ "Joey", "Ross", "Chandler", "Phoebe", "Monika", "Rachel" ]
        |> List.map (\name -> { name = name, score = 0 })



-- DECODER & ENCODER


targetValueContinentDecoder : Json.Decode.Decoder Flags.Continent
targetValueContinentDecoder =
    targetValue
        |> Json.Decode.andThen
            (\val ->
                case val of
                    "Europe" ->
                        Json.Decode.succeed Flags.Europe

                    -- "Africa" ->
                    --     Json.Decode.succeed Africa

                    -- "Asia" ->
                    --     Json.Decode.succeed Asia

                    -- "America" ->
                    --     Json.Decode.succeed America

                    -- "World" ->
                    --     Json.Decode.succeed World

                    _ ->
                        Json.Decode.fail ("Invalid Continent: " ++ val)
            )