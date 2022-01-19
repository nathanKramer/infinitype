module Texts.English200 exposing (corpus)

import Texts.English1k as E1k


corpus =
    { monosize = 0.5
    , name = "English 200"
    , words =
        E1k.corpus.words
            -- hack
            |> String.split "\n"
            |> List.filter (not << String.isEmpty)
            |> List.take 200
            |> String.join "\n"
    }
