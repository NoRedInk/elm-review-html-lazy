module Elm.Syntax.DenodeTest exposing (..)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Denode as Denode
import Expect
import Test exposing (Test, describe, test)


testFile =
    """
module A exposing (func, Test(..))

type alias Test = { name: String }

func : Int -> String
func x = "a"
"""


fileResult =
    Parser.parse testFile |> Result.map (\rawFile -> Processing.process Processing.init rawFile)


isSubsequence : List Char -> List Char -> Bool
isSubsequence subChars seqChars =
    case ( subChars, seqChars ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( sub :: subs, seq :: seqs ) ->
            if sub == seq then
                isSubsequence subs seqs

            else
                isSubsequence (sub :: subs) seqs


all : Test
all =
    describe "Elm.Syntax.Denode"
        [ test "String Parity" <|
            \() ->
                case fileResult of
                    Err _ ->
                        Expect.fail "Invalid test file"

                    Ok file ->
                        let
                            nodeChars =
                                file |> Debug.toString |> String.toList

                            denodeChars =
                                file |> Denode.denodeFile |> Debug.toString |> String.replace " ()" "" |> String.toList

                            _ =
                                file |> Denode.denodeFile |> Debug.log "file"
                        in
                        Expect.true "a" (isSubsequence denodeChars nodeChars)
        ]
