module Corpus exposing (..)

import Random exposing (Generator)
import Texts.English1k


type alias Corpus =
    { monosize : Float, name : String, words : String }


defaultCorpus : Corpus
defaultCorpus =
    Texts.English1k.corpus


wordBuffer : Int
wordBuffer =
    20


makeCorpus : String -> List String
makeCorpus words =
    words |> String.split "\n" |> List.filter (not << String.isEmpty)


randomWords : Int -> List String -> Generator (List String)
randomWords count words =
    let
        fallback =
            Maybe.withDefault "EMPTY_CORPUS" (List.head words)
    in
    Random.list count <| Random.uniform fallback words
