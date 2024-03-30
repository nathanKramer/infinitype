module Corpus exposing (..)

import Array
import Dict
import List.Extra as LE
import Random exposing (Generator)
import Texts.All exposing (texts)
import Texts.English1k


type alias Corpus =
    { monosize : Float, name : String, words : String }


defaultCorpus : Corpus
defaultCorpus =
    Texts.English1k.corpus


wordBuffer : Int
wordBuffer =
    10


makeCorpus : String -> List String
makeCorpus words =
    words |> String.split "\n" |> List.filter (not << String.isEmpty)


indexedCorpusList : List ( Int, ( String, Corpus ) )
indexedCorpusList =
    let
        itemsList =
            Dict.toList
                texts

        arrayOfTexts =
            itemsList |> Array.fromList

        items =
            Array.toIndexedList arrayOfTexts
    in
    items


getCorpus : Int -> Corpus
getCorpus idx =
    let
        findFn : ( Int, ( String, Corpus ) ) -> Bool
        findFn ( i, ( _, label ) ) =
            i == idx

        currentCorpus =
            case LE.find findFn indexedCorpusList of
                Just ( _, ( _, corpus ) ) ->
                    corpus

                Nothing ->
                    defaultCorpus
    in
    currentCorpus


randomWords : Int -> List String -> Generator (List String)
randomWords count words =
    let
        fallback =
            Maybe.withDefault "BUG" (List.head words)
    in
    Random.list count <| Random.uniform fallback words
