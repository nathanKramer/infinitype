module Texts.All exposing (..)

import Dict
import Texts.English1k as E1k
import Texts.JapaneseHiraganaCommon as J1
import Texts.JapaneseKatakanaCommon as J2
import Texts.MiscCode as Code
import Texts.MonkeyTypeQuotes as Quotes


texts =
    Dict.fromList
        [ ( E1k.corpus.name, E1k.corpus )
        , ( J1.corpus.name, J1.corpus )
        , ( J2.corpus.name, J2.corpus )
        , ( Code.corpus.name, Code.corpus )
        , ( Quotes.corpus.name, Quotes.corpus )
        ]
