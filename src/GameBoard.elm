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


type Msg
    = UpdateSetup Int Region
    | StartGame Int Region
    | PlayGame GameModel


type alias Player =
    { name : String
    , score : Int
    }


type alias Model =
    { gameScreen : GameScreen
    }


type alias GameModel =
    { players : List Player
    , activePlayer : Maybe Player -- TODO: think how can I get rid of Maybe
    , remainingFlags : List Flag
    , totalFlags : Int
    }


type Region
    = Europe
    | Africa
    | Asia
    | America
    | World


type GameScreen
    = Setup Int Region
    | Play GameModel



{--
new Model
  players
  activePlayer
  remainingFlags
  answeredFlags


views
 - question -> has current stats, hidden flag,
 - answer -> has current stats, revealed flag
 - results ->  has current stats, winner
 ViewQuestion Stats Flag
--}


init : ( Model, Cmd Msg )
init =
    ( Model <| Setup 0 World, Cmd.none )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, cmd ) =
    case msg of
        UpdateSetup numberOfPlayers region ->
            ( { model | gameScreen = Setup numberOfPlayers region }, cmd )

        StartGame numberOfPlayers region ->
            let
                players =
                    List.take numberOfPlayers availablePlayers

                activePlayer =
                    List.head players

                remainingFlags =
                    getFlagsForRegion region

                totalFlags =
                    List.length remainingFlags

                gameModel =
                    { players = players
                    , activePlayer = activePlayer
                    , remainingFlags = remainingFlags
                    , totalFlags = totalFlags
                    }
            in
            ( { gameScreen = Play gameModel }, Cmd.none )

        PlayGame gameModel ->
            ( model, Cmd.none )


view : ( Model, Cmd Msg ) -> Html Msg
view ( model, cmd ) =
    let
        viewBoard =
            case model.gameScreen of
                Setup numberOfPlayers region ->
                    viewSetup numberOfPlayers region

                Play gameModel ->
                    viewGame gameModel
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


viewSetup numberOfPlayers region =
    let
        buttons =
            List.range 1 5
                |> List.map
                    (\index ->
                        button
                            [ onClick (UpdateSetup index region)
                            , classList
                                [ ( "f6 link dim br2 ph3 pv2 mb2 dib white", True )
                                , ( "bg-dark-green", numberOfPlayers /= index )
                                , ( "bg-dark-blue", numberOfPlayers == index )
                                ]
                            ]
                            [ text <| String.fromInt index ]
                    )

        regionOptions =
            [ Europe, Africa, Asia, America, World ]
                |> List.map
                    (\newRegion ->
                        option
                            [ value <| regionToString newRegion ]
                            [ text <| regionToString newRegion ]
                    )
    in
    div []
        [ h1 [] [ text "How many players?" ]
        , div [] buttons
        , select
            [ on "change" (Json.Decode.map (UpdateSetup numberOfPlayers) targetValueRegionDecoder) ]
            regionOptions
        , div []
            [ button
                [ onClick <| StartGame numberOfPlayers region
                , disabled <| numberOfPlayers <= 0
                ]
                [ text "Start game" ]
            ]
        ]


viewGame gameModel =
    div [] []



-- HELPER FUNCITONS


availablePlayers =
    [ "Joey", "Ross", "Chandler", "Phoebe", "Monika", "Rachel" ]
        |> List.map (\name -> { name = name, score = 0 })



-- DECODER & ENCODER


targetValueRegionDecoder : Json.Decode.Decoder Region
targetValueRegionDecoder =
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
                        Json.Decode.fail ("Invalid Region: " ++ val)
            )


regionToString region =
    case region of
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
    , region : Region
    }


getFlagsForRegion region =
    flags
        |> List.filter (\f -> f.region == region)


flags : List Flag
flags =
    [ { imgUrl = "//flags.fmcdn.net/data/flags/normal/af.png"
      , countryName = "Afghanistan"
      , region = Asia
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/al.png"
      , countryName = "Albania"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/dz.png"
      , countryName = "Algeria"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ad.png"
      , countryName = "Andorra"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ao.png"
      , countryName = "Angola"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ag.png"
      , countryName = "Antigua and Barbuda"
      , region = Africa
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ar.png"
      , countryName = "Argentina"
      , region = America
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/am.png"
      , countryName = "Armenia"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/au.png"
      , countryName = "Australia"
      , region = Asia
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/at.png"
      , countryName = "Austria"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/az.png"
      , countryName = "Azerbaijan"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bs.png"
      , countryName = "The Bahamas"
      , region = America
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bh.png"
      , countryName = "Bahrain"
      , region = Asia
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bd.png"
      , countryName = "Bangladesh"
      , region = Asia
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bb.png"
      , countryName = "Barbados"
      , region = Asia
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/by.png"
      , countryName = "Belarus"
      , region = Europe
      }
    , { imgUrl = "//flags.fmcdn.net/data/flags/normal/be.png"
      , countryName = "Belgium"
      , region = Europe
      }

    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bz.png"
    --   , countryName = "Belize"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bj.png"
    --   , countryName = "Benin"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bt.png"
    --   , countryName = "Bhutan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bo.png"
    --   , countryName = "Bolivia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ba.png"
    --   , countryName = "Bosnia and Herzegovina"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bw.png"
    --   , countryName = "Botswana"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/br.png"
    --   , countryName = "Brazil"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bn.png"
    --   , countryName = "Brunei"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bg.png"
    --   , countryName = "Bulgaria"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bf.png"
    --   , countryName = "Burkina Faso"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/bi.png"
    --   , countryName = "Burundi"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kh.png"
    --   , countryName = "Cambodia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cm.png"
    --   , countryName = "Cameroon"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ca.png"
    --   , countryName = "Canada"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cv.png"
    --   , countryName = "Cape Verde"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cf.png"
    --   , countryName = "The Central African Republic"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/td.png"
    --   , countryName = "Chad"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cl.png"
    --   , countryName = "Chile"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/co.png"
    --   , countryName = "Colombia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/km.png"
    --   , countryName = "The Comoros"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ck.png"
    --   , countryName = "Cook Islands"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cr.png"
    --   , countryName = "Costa Rica"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ci.png"
    --   , countryName = "Cote d'Ivoire"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/hr.png"
    --   , countryName = "Croatia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cu.png"
    --   , countryName = "Cuba"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cy.png"
    --   , countryName = "Cyprus"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cz.png"
    --   , countryName = "The Czech Republic"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cd.png"
    --   , countryName = "The Democratic Republic of the Congo"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/dk.png"
    --   , countryName = "Denmark"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/dj.png"
    --   , countryName = "Djibouti"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/dm.png"
    --   , countryName = "Dominica"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/do.png"
    --   , countryName = "The Dominican Republic"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tl.png"
    --   , countryName = "East Timor"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ec.png"
    --   , countryName = "Ecuador"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/eg.png"
    --   , countryName = "Egypt"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sv.png"
    --   , countryName = "El Salvador"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gq.png"
    --   , countryName = "Equatorial Guinea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/er.png"
    --   , countryName = "Eritrea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ee.png"
    --   , countryName = "Estonia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/et.png"
    --   , countryName = "Ethiopia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/fj.png"
    --   , countryName = "Fiji"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/fi.png"
    --   , countryName = "Finland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/fr.png"
    --   , countryName = "France"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ga.png"
    --   , countryName = "Gabon"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gm.png"
    --   , countryName = "The Gambia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ge.png"
    --   , countryName = "Georgia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/de.png"
    --   , countryName = "Germany"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gh.png"
    --   , countryName = "Ghana"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gr.png"
    --   , countryName = "Greece"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gd.png"
    --   , countryName = "Grenada"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gt.png"
    --   , countryName = "Guatemala"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gn.png"
    --   , countryName = "Guinea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gw.png"
    --   , countryName = "Guinea-Bissau"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gy.png"
    --   , countryName = "Guyana"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ht.png"
    --   , countryName = "Haiti"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/hn.png"
    --   , countryName = "Honduras"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/hu.png"
    --   , countryName = "Hungary"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/is.png"
    --   , countryName = "Iceland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/in.png"
    --   , countryName = "India"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/id.png"
    --   , countryName = "Indonesia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ir.png"
    --   , countryName = "Iran"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/iq.png"
    --   , countryName = "Iraq"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ie.png"
    --   , countryName = "Ireland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/il.png"
    --   , countryName = "Israel"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/it.png"
    --   , countryName = "Italy"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/jm.png"
    --   , countryName = "Jamaica"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/jp.png"
    --   , countryName = "Japan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/jo.png"
    --   , countryName = "Jordan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kz.png"
    --   , countryName = "Kazakhstan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ke.png"
    --   , countryName = "Kenya"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ki.png"
    --   , countryName = "Kiribati"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ks.png"
    --   , countryName = "Kosovo"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kw.png"
    --   , countryName = "Kuwait"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kg.png"
    --   , countryName = "Kyrgyzstan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/la.png"
    --   , countryName = "Laos"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lv.png"
    --   , countryName = "Latvia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lb.png"
    --   , countryName = "Lebanon"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ls.png"
    --   , countryName = "Lesotho"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lr.png"
    --   , countryName = "Liberia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ly.png"
    --   , countryName = "Libya"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/li.png"
    --   , countryName = "Liechtenstein"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lt.png"
    --   , countryName = "Lithuania"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lu.png"
    --   , countryName = "Luxembourg"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mk.png"
    --   , countryName = "Macedonia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mg.png"
    --   , countryName = "Madagascar"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mw.png"
    --   , countryName = "Malawi"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/my.png"
    --   , countryName = "Malaysia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mv.png"
    --   , countryName = "Maldives"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ml.png"
    --   , countryName = "Mali"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mt.png"
    --   , countryName = "Malta"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mh.png"
    --   , countryName = "The Marshall Islands"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mr.png"
    --   , countryName = "Mauritania"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mu.png"
    --   , countryName = "Mauritius"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mx.png"
    --   , countryName = "Mexico"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/fm.png"
    --   , countryName = "Micronesia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/md.png"
    --   , countryName = "Moldova"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mc.png"
    --   , countryName = "Monaco"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mn.png"
    --   , countryName = "Mongolia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/me.png"
    --   , countryName = "Montenegro"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ma.png"
    --   , countryName = "Morocco"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mz.png"
    --   , countryName = "Mozambique"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/mm.png"
    --   , countryName = "Myanmar"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/na.png"
    --   , countryName = "Namibia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/nr.png"
    --   , countryName = "Nauru"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/np.png"
    --   , countryName = "Nepal"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/nl.png"
    --   , countryName = "The Netherlands"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/nz.png"
    --   , countryName = "New Zealand"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ni.png"
    --   , countryName = "Nicaragua"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ne.png"
    --   , countryName = "Niger"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ng.png"
    --   , countryName = "Nigeria"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/nu.png"
    --   , countryName = "Niue"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kp.png"
    --   , countryName = "North Korea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/no.png"
    --   , countryName = "Norway"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/om.png"
    --   , countryName = "Oman"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pk.png"
    --   , countryName = "Pakistan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pw.png"
    --   , countryName = "Palau"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pa.png"
    --   , countryName = "Panama"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pg.png"
    --   , countryName = "Papua New Guinea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/py.png"
    --   , countryName = "Paraguay"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cn.png"
    --   , countryName = "The People's Republic of China"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pe.png"
    --   , countryName = "Peru"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ph.png"
    --   , countryName = "The Philippines"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pl.png"
    --   , countryName = "Poland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/pt.png"
    --   , countryName = "Portugal"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/qa.png"
    --   , countryName = "Qatar"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tw.png"
    --   , countryName = "The Republic of China"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/cg.png"
    --   , countryName = "The Republic of the Congo"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ro.png"
    --   , countryName = "Romania"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ru.png"
    --   , countryName = "Russia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/rw.png"
    --   , countryName = "Rwanda"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kn.png"
    --   , countryName = "Saint Kitts and Nevis"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lc.png"
    --   , countryName = "Saint Lucia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/vc.png"
    --   , countryName = "Saint Vincent and the Grenadines"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ws.png"
    --   , countryName = "Samoa"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sm.png"
    --   , countryName = "San Marino"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/st.png"
    --   , countryName = "Sao Tome and Principe"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sa.png"
    --   , countryName = "Saudi Arabia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sn.png"
    --   , countryName = "Senegal"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/rs.png"
    --   , countryName = "Serbia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sc.png"
    --   , countryName = "The Seychelles"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sl.png"
    --   , countryName = "Sierra Leone"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sg.png"
    --   , countryName = "Singapore"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sk.png"
    --   , countryName = "Slovakia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/si.png"
    --   , countryName = "Slovenia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sb.png"
    --   , countryName = "The Solomon Islands"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/so.png"
    --   , countryName = "Somalia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/za.png"
    --   , countryName = "South Africa"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/kr.png"
    --   , countryName = "South Korea"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ss.png"
    --   , countryName = "South Sudan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/es.png"
    --   , countryName = "Spain"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/lk.png"
    --   , countryName = "Sri Lanka"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sd.png"
    --   , countryName = "Sudan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sr.png"
    --   , countryName = "Suriname"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sz.png"
    --   , countryName = "Swaziland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/se.png"
    --   , countryName = "Sweden"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ch.png"
    --   , countryName = "Switzerland"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/sy.png"
    --   , countryName = "Syria"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tj.png"
    --   , countryName = "Tajikistan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tz.png"
    --   , countryName = "Tanzania"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/th.png"
    --   , countryName = "Thailand"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tg.png"
    --   , countryName = "Togo"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/to.png"
    --   , countryName = "Tonga"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tt.png"
    --   , countryName = "Trinidad and Tobago"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tn.png"
    --   , countryName = "Tunisia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tr.png"
    --   , countryName = "Turkey"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tm.png"
    --   , countryName = "Turkmenistan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/tv.png"
    --   , countryName = "Tuvalu"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ug.png"
    --   , countryName = "Uganda"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ua.png"
    --   , countryName = "Ukraine"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ae.png"
    --   , countryName = "The United Arab Emirates"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/gb.png"
    --   , countryName = "The United Kingdom"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/us.png"
    --   , countryName = "The United States"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/uy.png"
    --   , countryName = "Uruguay"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/uz.png"
    --   , countryName = "Uzbekistan"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/vu.png"
    --   , countryName = "Vanuatu"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/va.png"
    --   , countryName = "The Vatican City"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ve.png"
    --   , countryName = "Venezuela"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/vn.png"
    --   , countryName = "Vietnam"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/eh.png"
    --   , countryName = "Western Sahara"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/ye.png"
    --   , countryName = "Yemen"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/zm.png"
    --   , countryName = "Zambia"
    --   }
    -- , { imgUrl = "//flags.fmcdn.net/data/flags/normal/zw.png"
    --   , countryName = "Zimbabwe"
    --   }
    ]
