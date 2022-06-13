module UseMemoizedLazyLambdaTest exposing (..)

import Review.Test
import Test exposing (Test, describe, test)
import UseMemoizedLazyLambda exposing (rule)


withHeader : String -> String
withHeader body =
    """
module A exposing (..)
import Html.Lazy as Lazy
import Html exposing (text)

""" ++ body


all : Test
all =
    describe "UseMemoizedLambda"
        [ test "Passes if lazy is memoized and uses a lambda function (1)" <|
            \_ ->
                withHeader "f = Lazy.lazy (\\t -> text)"
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "Passes if lazy is memoized and uses a lambda function (2)" <|
            \_ ->
                withHeader "f = Lazy.lazy2 (\\t i -> text)"
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "Fails if lazy application is not the top expression" <|
            \_ ->
                withHeader """
f = let g = Lazy.lazy view
     in g
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calls to lazy should be memoized at the top level of a view function with a lambda function argument."
                            , details = [ "See here" ]
                            , under = "Lazy.lazy"
                            }
                        ]
        , test "Fails if lazy arg is not lambda" <|
            \_ ->
                withHeader "f = Lazy.lazy x"
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calls to lazy should be memoized at the top level of a view function with a lambda function argument."
                            , details = [ "See here" ]
                            , under = "Lazy.lazy"
                            }
                        ]
        ]
