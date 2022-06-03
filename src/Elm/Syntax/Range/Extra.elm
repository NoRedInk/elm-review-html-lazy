module Elm.Syntax.Range.Extra exposing (recoverRange, recoverRanges)

import Array exposing (Array)
import Elm.Syntax.Range exposing (Range)


indexLineStarts : String -> Array Int
indexLineStarts string =
    indexLineStartsHelper string 0 []
        |> List.reverse
        |> (::) 0
        |> Array.fromList



-- Will not be the fastest approach, but the String module doesn't
-- really give me the tools I need to do this properly


indexLineStartsHelper : String -> Int -> List Int -> List Int
indexLineStartsHelper string index accum =
    if String.isEmpty string then
        accum

    else if String.startsWith "\u{000D}\n" string then
        indexLineStartsHelper (String.dropLeft 2 string) (index + 2) (index + 2 :: accum)

    else if String.startsWith "\n" string || String.startsWith "\u{000D}" string then
        indexLineStartsHelper (String.dropLeft 1 string) (index + 1) (index + 1 :: accum)

    else
        indexLineStartsHelper (String.dropLeft 1 string) (index + 1) accum


recoverHelper : String -> Array Int -> Range -> Maybe String
recoverHelper source rowStarts { start, end } =
    case ( Array.get start.row rowStarts, Array.get end.row rowStarts ) of
        ( Just startIndex, Just endIndex ) ->
            String.slice (startIndex + start.column) (endIndex + end.column) source
                |> Just

        _ ->
            Nothing


recoverRange : String -> Range -> Maybe String
recoverRange source range =
    recoverHelper source (indexLineStarts source) range


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


{-| Will be faster than recoverRange when recovering multiple ranges from the same text
-}
recoverRanges : String -> List Range -> Maybe (List String)
recoverRanges source ranges =
    let
        rowStarts =
            indexLineStarts source
    in
    List.map (recoverHelper source rowStarts) ranges
        |> combine
