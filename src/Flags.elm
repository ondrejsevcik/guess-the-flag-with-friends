module Flags exposing (..)

import Json.Decode as Decode

-- = Africa
-- | Asia
-- | North America
-- | Oceania
-- | South America
-- | All

type alias Flag = 
    { emojiFlag: String
    , countryName: String 
    }

type Continent
    = Europe

allContinents = [ Europe ]


continentToString continent =
    case continent of
        Europe ->
            "Europe"

getFlagsForContinent continent =
    case continent of
        Europe ->
            europeFlags
    

-- FLAGS DEFINITIONS

europeFlags =
    [ { emojiFlag = "🇦🇱", countryName = "Albania" }
    , { emojiFlag = "🇦🇩", countryName = "Andorra" }
    , { emojiFlag = "🇦🇹", countryName = "Austria" }
    , { emojiFlag = "🇧🇾", countryName = "Belarus" }
    , { emojiFlag = "🇧🇪", countryName = "Belgium" }
    , { emojiFlag = "🇧🇦", countryName = "Bosnia and Herzegovina" }
    , { emojiFlag = "🇧🇬", countryName = "Bulgaria" }
    , { emojiFlag = "🇭🇷", countryName = "Croatia" }
    , { emojiFlag = "🇨🇿", countryName = "Czech" }
    , { emojiFlag = "🇩🇰", countryName = "Denmark" }
    , { emojiFlag = "🇪🇪", countryName = "Estonia" }
    , { emojiFlag = "🇫🇮", countryName = "Finland" }
    , { emojiFlag = "🇫🇷", countryName = "France" }
    , { emojiFlag = "🇩🇪", countryName = "Germany" }
    , { emojiFlag = "🇬🇷", countryName = "Greece" }
    , { emojiFlag = "🇭🇺", countryName = "Hungary" }
    , { emojiFlag = "🇮🇸", countryName = "Iceland" }
    , { emojiFlag = "🇮🇪", countryName = "Ireland" }
    , { emojiFlag = "🇮🇹", countryName = "Italy" }
    , { emojiFlag = "🇽🇰", countryName = "Kosovo" }
    , { emojiFlag = "🇱🇻", countryName = "Latvia" }
    , { emojiFlag = "🇱🇮", countryName = "Liechtenstein" }
    , { emojiFlag = "🇱🇹", countryName = "Lithuania" }
    , { emojiFlag = "🇱🇺", countryName = "Luxembourg" }
    , { emojiFlag = "🇲🇰", countryName = "Macedonia" }
    , { emojiFlag = "🇲🇹", countryName = "Malta" }
    , { emojiFlag = "🇲🇩", countryName = "Moldova" }
    , { emojiFlag = "🇲🇨", countryName = "Monaco" }
    , { emojiFlag = "🇲🇪", countryName = "Montenegro" }
    , { emojiFlag = "🇳🇱", countryName = "Netherlands" }
    , { emojiFlag = "🇳🇴", countryName = "Norway" }
    , { emojiFlag = "🇵🇱", countryName = "Poland" }
    , { emojiFlag = "🇵🇹", countryName = "Portugal" }
    , { emojiFlag = "🇷🇴", countryName = "Romania" }
    , { emojiFlag = "🇷🇺", countryName = "Russia" }
    , { emojiFlag = "🇸🇲", countryName = "San Marino" }
    , { emojiFlag = "🇷🇸", countryName = "Serbia" }
    , { emojiFlag = "🇸🇰", countryName = "Slovakia" }
    , { emojiFlag = "🇸🇮", countryName = "Slovenia" }
    , { emojiFlag = "🇪🇸", countryName = "Spain" }
    , { emojiFlag = "🇸🇪", countryName = "Sweden" }
    , { emojiFlag = "🇨🇭", countryName = "Switzerland" }
    , { emojiFlag = "🇹🇷", countryName = "Turkey" }
    , { emojiFlag = "🇺🇦", countryName = "Ukraine" }
    , { emojiFlag = "🇬🇧", countryName = "United Kingdom" }
    , { emojiFlag = "🇻🇦", countryName = "Vatican City" }
    ]
