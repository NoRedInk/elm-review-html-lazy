module Elm.Syntax.Expression.ExtraTest exposing (..)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Denode as Denode
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Expression.Extra as Extra exposing (normalizeApplication)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Range.Extra exposing (recoverRanges)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


testExpressionCount : String -> Int -> (() -> Expectation)
testExpressionCount expString expectedCount =
    let
        prelude =
            """module A exposing (..)

func = """

        fileResult =
            Parser.parse (prelude ++ expString) |> Result.map (\rawFile -> Processing.process Processing.init rawFile)

        foldFunc _ count =
            count + 1

        expectation =
            case fileResult of
                Err deadEnds ->
                    Expect.fail ("Invalid test file: " ++ Debug.toString deadEnds)

                Ok { declarations } ->
                    case declarations of
                        [ Node _ (FunctionDeclaration { declaration }) ] ->
                            let
                                exp =
                                    (Node.value declaration).expression
                            in
                            Expect.equal (Extra.fold foldFunc 0 exp) expectedCount

                        _ ->
                            Expect.fail "Expected exactly one function declaration in module"
    in
    \_ -> expectation


foldTests : Test
foldTests =
    describe "fold"
        [ describe "Expression count checks"
            [ test "Simple Exp (1)" <|
                testExpressionCount "1" 1
            , test "Simple Exp (2)" <|
                testExpressionCount "\"asdf\"" 1
            , test "Application (1)" <|
                testExpressionCount "func 1" 3
            , test "Application (2)" <|
                testExpressionCount "func 1 2" 4
            , test "Operator (1)" <|
                testExpressionCount "1 + 1" 3
            , test "Operator (2)" <|
                testExpressionCount "1 + 1 + 1" 5
            , test "If (1)" <|
                testExpressionCount "if True then 1 else 2" 4
            , test "If (2)" <|
                testExpressionCount "if True then if False then 1 else 2 else 3" 7
            , test "Negation" <|
                testExpressionCount "-5" 2
            , test "Tuple (1)" <|
                testExpressionCount "(1, 2)" 3
            , test "Tuple (2)" <|
                testExpressionCount "(1, 2, 3)" 4
            , test "Tuple (3)" <|
                testExpressionCount "(1, (2, 3))" 5
            , test "Parenthesized Expression" <|
                testExpressionCount "(1)" 2
            , test "Let Expression (1)" <|
                testExpressionCount """
    let x = 1
     in x""" 3
            , test "Let Expression (2)" <|
                testExpressionCount """
    let x = 1
        y = 2
     in x + y""" 6
            , test "Case Expression (1)" <|
                testExpressionCount """
    case True of
        _ -> 1""" 3
            , test "Case Expression (2)" <|
                testExpressionCount """
    case True of
        False -> 1
        _ -> 2""" 4
            , test "Lambda Expression" <|
                testExpressionCount "\\x -> 1" 2
            , test "Record Expression (1)" <|
                testExpressionCount "{ x = 1 }" 2
            , test "Record Expression (2)" <|
                testExpressionCount "{ x = 1, y = 2}" 3
            , test "List Expression (1)" <|
                testExpressionCount "[]" 1
            , test "List Expression (2)" <|
                testExpressionCount "[1]" 2
            , test "List Expression (3)" <|
                testExpressionCount "[1, 2]" 3
            , test "Record Access" <|
                testExpressionCount "x.y" 2
            , test "Record Update (1)" <|
                testExpressionCount "{ x | y = 1 }" 2
            , test "Record Update (2)" <|
                testExpressionCount "{ x | y = 1, z = 2 }" 3
            ]
        ]


normalizeTest : String -> List String -> (() -> Expectation)
normalizeTest expString expectedList =
    \_ ->
        let
            prefix =
                """
module A exposing (..)
    
func = """

            source =
                prefix ++ expString

            fileResult =
                Parser.parse source |> Result.map (\rawFile -> Processing.process Processing.init rawFile)
        in
        case fileResult of
            Err deadEnds ->
                Expect.fail ("Invalid test file: " ++ Debug.toString deadEnds)

            Ok { declarations } ->
                case declarations of
                    [ Node _ (FunctionDeclaration { declaration }) ] ->
                        let
                            exp =
                                (Node.value declaration).expression

                            maybeArgs =
                                normalizeApplication exp
                                    |> List.map (\node -> Node.range node)
                                    |> recoverRanges source
                        in
                        case maybeArgs of
                            Just args ->
                                Expect.equalLists args expectedList

                            Nothing ->
                                Expect.fail "Failed to recover ranges :("

                    _ ->
                        Expect.fail "Expected exactly one function declaration in module"



-- a b -> [a, b]
-- a b c -> [a, b, c]
-- a <| b -> [a, b]
-- b |> a -> [a, b]
-- a (b) -> [a, b]


normalizeApplicationTests : Test
normalizeApplicationTests =
    describe "normalizeApplication"
        [ test "Simple application (1)" <|
            normalizeTest "a b" [ "a", "b" ]
        , test "Simple application (2)" <|
            normalizeTest "a b c" [ "a", "b", "c" ]
        , test "Left pizza (1)" <|
            normalizeTest "a <| b" [ "a", "b" ]
        , test "Left pizza (2)" <|
            normalizeTest "a b <| c" [ "a", "b", "c" ]
        , test "Left pizza (3)" <|
            normalizeTest "a <| b <| c" [ "a", "b", "c" ]
        , test "Right pizza (1)" <|
            normalizeTest "a |> b" [ "b", "a" ]
        , test "Right pizza (2)" <|
            normalizeTest "a |> b c" [ "b", "c", "a" ]
        , test "Parenthesized (1)" <|
            normalizeTest "a (b)" [ "a", "(b)" ]
        , test "Parenthesized (2)" <|
            normalizeTest "(a b) c" [ "a", "b", "c" ]
        , test "Parenthesized (3)" <|
            normalizeTest "a (b c)" [ "a", "(b c)" ]
        , test "Parenthesized (4)" <|
            normalizeTest "(a b)" [ "a", "b" ]
        ]


all : Test
all =
    describe "Elm.Syntax.Expression.Extra"
        [ foldTests
        , normalizeApplicationTests
        ]
