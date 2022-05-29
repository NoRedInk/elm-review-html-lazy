module Elm.Syntax.DenodeTest exposing (..)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Denode as Denode
import Expect
import Test exposing (Test, describe, test)


testFile : String
testFile =
    """
module A exposing (func, Test(..))

import List exposing (map)
import Dict exposing (..)
import X.Y as Y exposing (TypeName(..))

type alias Test = { name: String }

func : Int -> String
func x = 
    let (a, b) = ((), "a" ++ "b")
        c = func <| x - 1
        d = if x == 1 then (-7) else 8
        e = 8.0
        f = 'c'
        g = case (x, c) of 
            (1, "a") -> True
            (1, _) -> False
            _ -> False
        h = \\i -> i + 1
        i = { a = 1, b = "a" }
        j = { i | a = 2 }
        k = 1 :: []
        ((l :: m) as n) = k
        [o, p] = k
     in "a"
"""


fileResult =
    Parser.parse testFile |> Result.map (\rawFile -> Processing.process Processing.init rawFile)


{-|

    Test if one list of characters is a subsequence of another.

    Because the printed output of any syntax tree with nodes will be strictly richer than
    the printed tree without nodes, we can use this to test that we have correctly duplicated the tree.

    isSubsequence "bd" "abcde" == True
    isSubsequence "db" "abcde" == False

-}
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
                    Err deadEnds ->
                        Expect.fail ("Invalid test file: " ++ Debug.toString deadEnds)

                    Ok file ->
                        let
                            nodeChars =
                                file |> Debug.toString |> String.toList

                            denodeChars =
                                file |> Denode.denodeFile |> Debug.toString |> String.replace " ()" "" |> String.toList
                        in
                        Expect.true "a" (isSubsequence denodeChars nodeChars)
        ]
