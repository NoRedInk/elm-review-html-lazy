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
                withHeader """
f : String -> Html a
f = Lazy.lazy (\\t -> text t)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "Passes if lazy is memoized and uses a lambda function (2)" <|
            \_ ->
                withHeader """
f : String -> Int -> Html
f = Lazy.lazy2 (\\t i -> text t)
"""
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
        , test "Fails if lazy is memoized and uses a lambda function without a type signature" <|
            \_ ->
                withHeader "f = Lazy.lazy (\\t -> text t)"
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This function which calls lazy is missing a type signature."
                            , details = [ "A type signature improves clarity and can help other rules do more powerful analysis." ]
                            , under = "f = Lazy.lazy (\\t -> text t)"
                            }
                        ]
        , test "Fails if lazy is memoized and uses a lambda function with a higher airity than lazy" <|
            \_ ->
                withHeader """
f : Int -> String -> Html a
f = Lazy.lazy2 (\\i -> text)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The airity (number of arguments) of this lambda expression should match the number of arguments expected by the lazy function."
                            , details =
                                [ "This lambda expression accepts 1 argument, but the lazy function is lazy2."
                                , "Giving names to all arguments can give clarity to future readers of this code."
                                ]
                            , under = "\\i -> text"
                            }
                        ]
        ]
