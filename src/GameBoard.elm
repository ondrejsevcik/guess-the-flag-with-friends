module GameBoard exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra


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
    = Setup Int Continent
    | Question GameModel
    | Answer GameModel
    | Error String
    | Results (List Player)


type alias GameModel =
    { otherPlayers : List Player
    , activePlayer : Player
    , remainingFlags : List Flag
    , totalFlags : Int
    }


type alias Player =
    { name : String
    , score : Int
    }


type Msg
    = UpdateSetup Int Continent
    | StartGame Int Continent
    | ShowAnswer GameModel
    | MarkAsGood GameModel
    | MarkAsBad GameModel


type Continent
    = Europe
    | Africa
    | Asia
    | America
    | World


init : ( Model, Cmd Msg )
init =
    ( Model <| Setup 0 World, Cmd.none )


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
                                    getFlagsForContinent continent

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
            [ Europe, Africa, Asia, America, World ]
                |> List.map
                    (\continent ->
                        option
                            [ value <| continentToString continent
                            , selected (continent == selectedContinent)
                            ]
                            [ text <| continentToString continent ]
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
        flagUrl =
            case List.head gameModel.remainingFlags of
                Just flag ->
                    flag.imgUrl

                Nothing ->
                    ""
    in
    div
        []
        [ viewStats gameModel
        , div [] [ text "......." ]
        , div []
            [ img [ src flagUrl ] []
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
        ( flagUrl, countryName ) =
            case List.head gameModel.remainingFlags of
                Just flag ->
                    ( flag.imgUrl, flag.countryName )

                Nothing ->
                    ( "", "" )
    in
    div
        []
        [ viewStats gameModel
        , div [] [ text countryName ]
        , div []
            [ img [ src flagUrl ] []
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


targetValueContinentDecoder : Json.Decode.Decoder Continent
targetValueContinentDecoder =
    targetValue
        |> Json.Decode.andThen
            (\val ->
                case val of
                    "Europe" ->
                        Json.Decode.succeed Europe

                    "Africa" ->
                        Json.Decode.succeed Africa

                    "Asia" ->
                        Json.Decode.succeed Asia

                    "America" ->
                        Json.Decode.succeed America

                    "World" ->
                        Json.Decode.succeed World

                    _ ->
                        Json.Decode.fail ("Invalid Continent: " ++ val)
            )


continentToString continent =
    case continent of
        Europe ->
            "Europe"

        Africa ->
            "Africa"

        Asia ->
            "Asia"

        America ->
            "America"

        World ->
            "World"



--- FLAGS stuff


type alias Flag =
    { imgUrl : String
    , countryName : String
    , continent : Continent
    }


getFlagsForContinent continent =
    flags
        |> List.filter (\f -> f.continent == continent)



-- TODO: go through a list and assign a correct continent


flags : List Flag
flags =
    [ { imgUrl = "https://flags.fmcdn.net/data/flags/normal/af.png"
      , countryName = "Afghanistan"
      , continent = Asia
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/al.png"
      , countryName = "Albania"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/dz.png"
      , countryName = "Algeria"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ad.png"
      , countryName = "Andorra"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ao.png"
      , countryName = "Angola"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ag.png"
      , countryName = "Antigua and Barbuda"
      , continent = Africa
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ar.png"
      , countryName = "Argentina"
      , continent = America
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/am.png"
      , countryName = "Armenia"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/au.png"
      , countryName = "Australia"
      , continent = Asia
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/at.png"
      , countryName = "Austria"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/az.png"
      , countryName = "Azerbaijan"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bs.png"
      , countryName = "The Bahamas"
      , continent = America
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bh.png"
      , countryName = "Bahrain"
      , continent = Asia
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bd.png"
      , countryName = "Bangladesh"
      , continent = Asia
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bb.png"
      , countryName = "Barbados"
      , continent = Asia
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/by.png"
      , countryName = "Belarus"
      , continent = Europe
      }
    , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/be.png"
      , countryName = "Belgium"
      , continent = Europe
      }

    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bz.png"
    --   , countryName = "Belize"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bj.png"
    --   , countryName = "Benin"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bt.png"
    --   , countryName = "Bhutan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bo.png"
    --   , countryName = "Bolivia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ba.png"
    --   , countryName = "Bosnia and Herzegovina"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bw.png"
    --   , countryName = "Botswana"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/br.png"
    --   , countryName = "Brazil"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bn.png"
    --   , countryName = "Brunei"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bg.png"
    --   , countryName = "Bulgaria"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bf.png"
    --   , countryName = "Burkina Faso"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/bi.png"
    --   , countryName = "Burundi"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kh.png"
    --   , countryName = "Cambodia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cm.png"
    --   , countryName = "Cameroon"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ca.png"
    --   , countryName = "Canada"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cv.png"
    --   , countryName = "Cape Verde"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cf.png"
    --   , countryName = "The Central African Republic"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/td.png"
    --   , countryName = "Chad"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cl.png"
    --   , countryName = "Chile"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/co.png"
    --   , countryName = "Colombia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/km.png"
    --   , countryName = "The Comoros"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ck.png"
    --   , countryName = "Cook Islands"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cr.png"
    --   , countryName = "Costa Rica"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ci.png"
    --   , countryName = "Cote d'Ivoire"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/hr.png"
    --   , countryName = "Croatia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cu.png"
    --   , countryName = "Cuba"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cy.png"
    --   , countryName = "Cyprus"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cz.png"
    --   , countryName = "The Czech Republic"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cd.png"
    --   , countryName = "The Democratic Republic of the Congo"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/dk.png"
    --   , countryName = "Denmark"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/dj.png"
    --   , countryName = "Djibouti"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/dm.png"
    --   , countryName = "Dominica"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/do.png"
    --   , countryName = "The Dominican Republic"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tl.png"
    --   , countryName = "East Timor"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ec.png"
    --   , countryName = "Ecuador"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/eg.png"
    --   , countryName = "Egypt"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sv.png"
    --   , countryName = "El Salvador"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gq.png"
    --   , countryName = "Equatorial Guinea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/er.png"
    --   , countryName = "Eritrea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ee.png"
    --   , countryName = "Estonia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/et.png"
    --   , countryName = "Ethiopia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/fj.png"
    --   , countryName = "Fiji"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/fi.png"
    --   , countryName = "Finland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/fr.png"
    --   , countryName = "France"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ga.png"
    --   , countryName = "Gabon"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gm.png"
    --   , countryName = "The Gambia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ge.png"
    --   , countryName = "Georgia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/de.png"
    --   , countryName = "Germany"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gh.png"
    --   , countryName = "Ghana"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gr.png"
    --   , countryName = "Greece"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gd.png"
    --   , countryName = "Grenada"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gt.png"
    --   , countryName = "Guatemala"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gn.png"
    --   , countryName = "Guinea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gw.png"
    --   , countryName = "Guinea-Bissau"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gy.png"
    --   , countryName = "Guyana"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ht.png"
    --   , countryName = "Haiti"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/hn.png"
    --   , countryName = "Honduras"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/hu.png"
    --   , countryName = "Hungary"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/is.png"
    --   , countryName = "Iceland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/in.png"
    --   , countryName = "India"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/id.png"
    --   , countryName = "Indonesia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ir.png"
    --   , countryName = "Iran"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/iq.png"
    --   , countryName = "Iraq"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ie.png"
    --   , countryName = "Ireland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/il.png"
    --   , countryName = "Israel"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/it.png"
    --   , countryName = "Italy"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/jm.png"
    --   , countryName = "Jamaica"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/jp.png"
    --   , countryName = "Japan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/jo.png"
    --   , countryName = "Jordan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kz.png"
    --   , countryName = "Kazakhstan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ke.png"
    --   , countryName = "Kenya"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ki.png"
    --   , countryName = "Kiribati"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ks.png"
    --   , countryName = "Kosovo"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kw.png"
    --   , countryName = "Kuwait"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kg.png"
    --   , countryName = "Kyrgyzstan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/la.png"
    --   , countryName = "Laos"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lv.png"
    --   , countryName = "Latvia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lb.png"
    --   , countryName = "Lebanon"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ls.png"
    --   , countryName = "Lesotho"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lr.png"
    --   , countryName = "Liberia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ly.png"
    --   , countryName = "Libya"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/li.png"
    --   , countryName = "Liechtenstein"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lt.png"
    --   , countryName = "Lithuania"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lu.png"
    --   , countryName = "Luxembourg"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mk.png"
    --   , countryName = "Macedonia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mg.png"
    --   , countryName = "Madagascar"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mw.png"
    --   , countryName = "Malawi"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/my.png"
    --   , countryName = "Malaysia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mv.png"
    --   , countryName = "Maldives"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ml.png"
    --   , countryName = "Mali"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mt.png"
    --   , countryName = "Malta"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mh.png"
    --   , countryName = "The Marshall Islands"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mr.png"
    --   , countryName = "Mauritania"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mu.png"
    --   , countryName = "Mauritius"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mx.png"
    --   , countryName = "Mexico"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/fm.png"
    --   , countryName = "Micronesia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/md.png"
    --   , countryName = "Moldova"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mc.png"
    --   , countryName = "Monaco"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mn.png"
    --   , countryName = "Mongolia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/me.png"
    --   , countryName = "Montenegro"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ma.png"
    --   , countryName = "Morocco"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mz.png"
    --   , countryName = "Mozambique"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/mm.png"
    --   , countryName = "Myanmar"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/na.png"
    --   , countryName = "Namibia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/nr.png"
    --   , countryName = "Nauru"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/np.png"
    --   , countryName = "Nepal"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/nl.png"
    --   , countryName = "The Netherlands"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/nz.png"
    --   , countryName = "New Zealand"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ni.png"
    --   , countryName = "Nicaragua"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ne.png"
    --   , countryName = "Niger"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ng.png"
    --   , countryName = "Nigeria"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/nu.png"
    --   , countryName = "Niue"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kp.png"
    --   , countryName = "North Korea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/no.png"
    --   , countryName = "Norway"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/om.png"
    --   , countryName = "Oman"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pk.png"
    --   , countryName = "Pakistan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pw.png"
    --   , countryName = "Palau"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pa.png"
    --   , countryName = "Panama"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pg.png"
    --   , countryName = "Papua New Guinea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/py.png"
    --   , countryName = "Paraguay"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cn.png"
    --   , countryName = "The People's Republic of China"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pe.png"
    --   , countryName = "Peru"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ph.png"
    --   , countryName = "The Philippines"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pl.png"
    --   , countryName = "Poland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/pt.png"
    --   , countryName = "Portugal"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/qa.png"
    --   , countryName = "Qatar"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tw.png"
    --   , countryName = "The Republic of China"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/cg.png"
    --   , countryName = "The Republic of the Congo"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ro.png"
    --   , countryName = "Romania"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ru.png"
    --   , countryName = "Russia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/rw.png"
    --   , countryName = "Rwanda"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kn.png"
    --   , countryName = "Saint Kitts and Nevis"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lc.png"
    --   , countryName = "Saint Lucia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/vc.png"
    --   , countryName = "Saint Vincent and the Grenadines"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ws.png"
    --   , countryName = "Samoa"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sm.png"
    --   , countryName = "San Marino"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/st.png"
    --   , countryName = "Sao Tome and Principe"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sa.png"
    --   , countryName = "Saudi Arabia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sn.png"
    --   , countryName = "Senegal"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/rs.png"
    --   , countryName = "Serbia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sc.png"
    --   , countryName = "The Seychelles"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sl.png"
    --   , countryName = "Sierra Leone"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sg.png"
    --   , countryName = "Singapore"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sk.png"
    --   , countryName = "Slovakia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/si.png"
    --   , countryName = "Slovenia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sb.png"
    --   , countryName = "The Solomon Islands"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/so.png"
    --   , countryName = "Somalia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/za.png"
    --   , countryName = "South Africa"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/kr.png"
    --   , countryName = "South Korea"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ss.png"
    --   , countryName = "South Sudan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/es.png"
    --   , countryName = "Spain"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/lk.png"
    --   , countryName = "Sri Lanka"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sd.png"
    --   , countryName = "Sudan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sr.png"
    --   , countryName = "Suriname"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sz.png"
    --   , countryName = "Swaziland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/se.png"
    --   , countryName = "Sweden"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ch.png"
    --   , countryName = "Switzerland"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/sy.png"
    --   , countryName = "Syria"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tj.png"
    --   , countryName = "Tajikistan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tz.png"
    --   , countryName = "Tanzania"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/th.png"
    --   , countryName = "Thailand"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tg.png"
    --   , countryName = "Togo"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/to.png"
    --   , countryName = "Tonga"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tt.png"
    --   , countryName = "Trinidad and Tobago"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tn.png"
    --   , countryName = "Tunisia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tr.png"
    --   , countryName = "Turkey"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tm.png"
    --   , countryName = "Turkmenistan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/tv.png"
    --   , countryName = "Tuvalu"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ug.png"
    --   , countryName = "Uganda"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ua.png"
    --   , countryName = "Ukraine"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ae.png"
    --   , countryName = "The United Arab Emirates"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/gb.png"
    --   , countryName = "The United Kingdom"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/us.png"
    --   , countryName = "The United States"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/uy.png"
    --   , countryName = "Uruguay"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/uz.png"
    --   , countryName = "Uzbekistan"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/vu.png"
    --   , countryName = "Vanuatu"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/va.png"
    --   , countryName = "The Vatican City"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ve.png"
    --   , countryName = "Venezuela"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/vn.png"
    --   , countryName = "Vietnam"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/eh.png"
    --   , countryName = "Western Sahara"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/ye.png"
    --   , countryName = "Yemen"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/zm.png"
    --   , countryName = "Zambia"
    --   }
    -- , { imgUrl = "https://flags.fmcdn.net/data/flags/normal/zw.png"
    --   , countryName = "Zimbabwe"
    --   }
    ]
