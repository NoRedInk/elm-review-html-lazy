module NoEqualityBreakingLazyArgsTest exposing (all)

import NoEqualityBreakingLazyArgs exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


importTests : Test
importTests =
    let
        badLambda =
            "(\\t -> text t)"

        hasError source =
            ("module A exposing (..)\n" ++ source)
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

x f = Html.Lazy.lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported unaliased (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy

x f = Html.Styled.Lazy.lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported aliased (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy as Lazy

x f = Lazy.lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported aliased (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy as Lazy

x f = Lazy.lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported explicitly (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy exposing (lazy)

x f = lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported explicitly (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy exposing (lazy)

x f = lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported exposing all (Html.Lazy)" <|
            \() ->
                """
import Html.Lazy exposing (..)

x f = lazy (\\t -> text t) f
"""
                    |> hasError
        , test "should identify the lazy function when module imported exposing all (Html.Styled.Lazy)" <|
            \() ->
                """
import Html.Styled.Lazy exposing (..)

x f = lazy (\\t -> text t) f
"""
                    |> hasError
        ]


firstArgumentTests : Test
firstArgumentTests =
    let
        header =
            """
module A exposing (..)
import Html.Lazy exposing (lazy, lazy2)
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
x t = lazy text t
"""
                    |> runExpectNoError
        , test "reports no error for top-level module function" <|
            \() ->
                """
toText t = text t
x t = lazy toText t
"""
                    |> runExpectNoError
        , test "reports no error for top-level module function reference" <|
            \() ->
                """
toText = text
x t = lazy toText t
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
x t = case (text, 7) of
    (toText, _) -> lazy toText t
"""
                    |> runExpectNoError
        , test "should report an error for an inline lambda expression" <|
            \() ->
                """
x t = lazy (\\s -> text s) t            
"""
                    |> runExpectErrorUnder "(\\s -> text s)"
        , test "should report an error for an inline function application expression" <|
            \() ->
                """
y s1 s2 = text (s1 ++ s2)

x t = lazy (y "Sample ") t           
"""
                    |> runExpectErrorUnder """(y "Sample ")"""
        ]


extraArgumentsTests : Test
extraArgumentsTests =
    let
        header =
            """
module A exposing (..)
import Html.Lazy exposing (lazy2)
import Html exposing (text)

    """

        runExpectErrorUnder under message source =
            (header ++ source)
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = [ "See <TODO: link>" ]
                        , under = under
                        }
                    ]

        -- runExpectNoError source =
        --     (header ++ source)
        --         |> Review.Test.run rule
        --         |> Review.Test.expectNoErrors
    in
    describe "Extra arguments"
        [ test "Fails if arg is a lambda" <|
            \_ ->
                """
x t = lazy2 text (\\_ -> "Hello") t
"""
                    |> runExpectErrorUnder "\\_ -> \"Hello\"" "Lamba expressions are not allowed in arguments to Html.lazy"
        , test "Fails if arg is tuple construction" <|
            \_ ->
                """
x t = lazy2 viewTuple (1,2) t
"""
                    |> runExpectErrorUnder "(1,2)" "Tuple constructions are not allowed in arguments to Html.lazy"
        , test "Fails if arg is record construction" <|
            \_ ->
                """
x t = lazy2 viewTuple { x = 1 } t               
"""
                    |> runExpectErrorUnder "{ x = 1 }" "Record constructions are not allowed in arguments to lazy"
        , test "Fails if arg is list construction" <|
            \_ ->
                """
x t = lazy2 viewTuple [1] t               
"""
                    |> runExpectErrorUnder "[1]" "List constructions are not allowed in arguments to lazy"
        , test "Fails if arg is list cons" <|
            \_ ->
                """
x t = lazy2 viewTuple (1 :: []) t               
"""
                    |> runExpectErrorUnder "1 :: []" "List cons are not allowed in arguments to lazy"
        ]


all : Test
all =
    describe "NoEqualityBreakingLazyArgs"
        [ importTests
        , firstArgumentTests
        , extraArgumentsTests
        ]
