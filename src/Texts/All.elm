module Texts.All exposing (..)

import Dict
import Texts.English10k as E10k
import Texts.English1k as E1k
import Texts.English200 as E200
import Texts.JapaneseHiraganaCommon as J1
import Texts.JapaneseKanjiCommon as J0
import Texts.JapaneseKatakanaCommon as J2
import Texts.MiscCode as Code
import Texts.MonkeyTypeQuotes as Quotes


texts =
    Dict.fromList
        [ ( E200.corpus.name, E200.corpus )
        , ( E1k.corpus.name, E1k.corpus )
        , ( E10k.corpus.name, E10k.corpus )
        , ( J0.corpus.name, J0.corpus )
        , ( J1.corpus.name, J1.corpus )
        , ( J2.corpus.name, J2.corpus )
        , ( Code.corpus.name, Code.corpus )
        , ( Quotes.corpus.name, Quotes.corpus )
        ]
