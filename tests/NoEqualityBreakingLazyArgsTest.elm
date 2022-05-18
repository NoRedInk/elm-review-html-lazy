module NoEqualityBreakingLazyArgsTest exposing (all)

import NoEqualityBreakingLazyArgs exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


importTests : Test
importTests =
    let
        badLambda =
            "(\\s -> text s)"

        hasError source =
            ("module A exposing (..)\n" ++ source ++ " " ++ badLambda)
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "This must be a top level function in order to be properly optimized by lazy"
                        , details = [ "Do this" ]
                        , under = badLambda
                        }
                    ]
    in
    describe "Import Tests"
        [ test "should identify the lazy function when module imported unaliased (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy

x = Html.Lazy.lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported unaliased (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy

x = Html.Styled.Lazy.lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported aliased (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy as Lazy

x = Lazy.lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported aliased (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy as Lazy

x = Lazy.lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported explicitly (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy exposing (lazy)

x = lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported explicitly (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy exposing (lazy)

x = lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported exposing all (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy exposing (..)

x = lazy
"""
                    |> hasError
        , test "should identify the lazy function when module imported exposing all (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy exposing (..)

x = lazy
"""
                    |> hasError
        ]


firstArgumentTests : Test
firstArgumentTests =
    let
        header =
            """
module A exposing (..)
import Html.Lazy exposing (lazy)
import Html exposing (text) 

    """

        runExpectErrorUnder under source =
            (header ++ source)
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "This must be a top level function in order to be properly optimized by lazy"
                        , details = [ "Do this" ]
                        , under = under
                        }
                    ]

        runExpectNoError source =
            (header ++ source)
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    in
    describe "First lazy argument"
        [ test "reports no error for imported function" <|
            \() ->
                """
x = lazy text "Sample Text"
"""
                    |> runExpectNoError
        , test "reports no error for top-level module function" <|
            \() ->
                """
toText t = text t
x = lazy toText "Sample Text"
"""
                    |> runExpectNoError
        , test "reports no error for top-level module function reference" <|
            \() ->
                """
toText = text
x = lazy toText "Sample Text"
"""
                    |> runExpectNoError
        , test "reports no error for function argument " <|
            \() ->
                """
x toText = lazy toText "Sample Text"
"""
                    |> runExpectNoError
        , test "reports no error for bound case pattern function reference" <|
            \() ->
                """
x = case (text, 7) of
    (toText, _) -> lazy toText "Sample Text"
"""
                    |> runExpectNoError
        , test "should report an error for an inline lambda expression" <|
            \() ->
                """
x = lazy (\\s -> text s) "Sample Text"            
"""
                    |> runExpectErrorUnder "(\\s -> text s)"
        , test "should report an error for an inline function application expression" <|
            \() ->
                """
y s1 s2 = text (s1 ++ s2)

x = lazy (y "Sample ") "Text"            
"""
                    |> runExpectErrorUnder """(y "Sample ")"""
        ]


all : Test
all =
    describe "NoEqualityBreakingLazyArgs"
        [ importTests
        , firstArgumentTests
        ]
