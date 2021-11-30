module Texts.All exposing (..)

import Dict
import Texts.English1k as E1k
import Texts.JapaneseHiraganaCommon as J1
import Texts.JapaneseKatakanaCommon as J2


texts =
    Dict.fromList
        [ ( E1k.corpus.name, E1k.corpus )
        , ( J1.corpus.name, J1.corpus )
        , ( J2.corpus.name, J2.corpus )
        ]
