module Elm.Syntax.Range.Extra exposing (lookupRange, lookupRanges)

{-| Provides helpers for working with Ranges produced by `elm-syntax`

@docs lookupRange, lookupRanges

-}

import Array exposing (Array)
import Elm.Syntax.Range exposing (Range)


indexLineStarts : String -> Array Int
indexLineStarts string =
    indexLineStartsHelper (String.toList string) 0 []
        |> List.reverse
        |> (::) 0
        |> Array.fromList


indexLineStartsHelper : List Char -> Int -> List Int -> List Int
indexLineStartsHelper string index accum =
    case string of
        '\u{000D}' :: '\n' :: rest ->
            indexLineStartsHelper rest (index + 2) (index + 2 :: accum)

        '\n' :: rest ->
            indexLineStartsHelper rest (index + 1) (index + 1 :: accum)

        '\u{000D}' :: rest ->
            indexLineStartsHelper rest (index + 1) (index + 1 :: accum)

        _ :: rest ->
            indexLineStartsHelper rest (index + 1) accum

        _ ->
            accum


{-| Borrowed from elm-community/maybe-extra
-}
combine : List (Maybe a) -> Maybe (List a)
combine list =
    combineHelp list []


combineHelp : List (Maybe a) -> List a -> Maybe (List a)
combineHelp list acc =
    case list of
        head :: tail ->
            case head of
                Just a ->
                    combineHelp tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)


recoverHelper : String -> Array Int -> Range -> Maybe String
recoverHelper source rowStarts { start, end } =
    case ( Array.get (start.row - 1) rowStarts, Array.get (end.row - 1) rowStarts ) of
        ( Just startIndex, Just endIndex ) ->
            String.slice (startIndex + start.column - 1) (endIndex + end.column - 1) source
                |> Just

        _ ->
            Nothing


{-| Lookup the string pointed to by a range in the source it was originally generated from.

    lookupRange "1 + 2"
        { start =
            { row = 1, column = 1 }
        , end =
            { row = 1, column = 2 }
        }
        == "1"

Note that if the range is out of bounds of the source text `lookupRange` will return nothing.
This can only happen if the range was not produced from the provided source text.

-}
lookupRange : String -> Range -> Maybe String
lookupRange source range =
    recoverHelper source (indexLineStarts source) range


{-| Use this instead of `recoverRange` when trying to recover multiple ranges from the same source text.
It only indexes the source once and thus will be much more efficient.
-}
lookupRanges : String -> List Range -> Maybe (List String)
lookupRanges source ranges =
    let
        rowStarts =
            indexLineStarts source
    in
    List.map (recoverHelper source rowStarts) ranges
        |> combine
