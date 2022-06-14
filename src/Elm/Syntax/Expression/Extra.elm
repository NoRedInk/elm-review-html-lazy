module Elm.Syntax.Expression.Extra exposing (fold, normalizeApplication)

{-| Provides helpers for working with the `Expression` type from `Elm.Syntax.Expression`

@docs fold, normalizeApplication

-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))


foldHelper : (Node Expression -> a -> a) -> a -> List (Node Expression) -> a
foldHelper function accum stack =
    case stack of
        [] ->
            accum

        expr :: stackTail ->
            let
                newStack =
                    case Node.value expr of
                        Application exprs ->
                            exprs

                        OperatorApplication _ _ leftExp rightExp ->
                            [ leftExp, rightExp ]

                        IfBlock condExp trueExp falseExp ->
                            [ condExp, trueExp, falseExp ]

                        Negation exp ->
                            [ exp ]

                        TupledExpression exps ->
                            exps

                        ParenthesizedExpression exp ->
                            [ exp ]

                        LetExpression { declarations, expression } ->
                            let
                                mapLetDeclarations (Node _ letDeclaration) =
                                    case letDeclaration of
                                        LetFunction { declaration } ->
                                            (Node.value declaration).expression

                                        LetDestructuring _ exp ->
                                            exp
                            in
                            List.map mapLetDeclarations declarations ++ [ expression ]

                        CaseExpression { expression, cases } ->
                            expression :: List.map Tuple.second cases

                        LambdaExpression { expression } ->
                            [ expression ]

                        RecordExpr recordSetters ->
                            List.map (Node.value >> Tuple.second) recordSetters

                        ListExpr exps ->
                            exps

                        RecordAccess exp _ ->
                            [ exp ]

                        RecordUpdateExpression _ recordSetters ->
                            List.map (Node.value >> Tuple.second) recordSetters

                        _ ->
                            []
            in
            foldHelper function (function expr accum) (newStack ++ stackTail)


{-| Folds an expression tree from the top to the bottom. The children of any given expression node will be visited depth first from left to right.

Assume that we have a function that can parse an Elm expression fragment `e : String -> Node Expression`

    e "1 + (2 - 3)"
        |> fold
            (\(Node _ exp) accum ->
                case exp of
                    OperatorApplication op _ _ _ ->
                        accum ++ [ op ]

                    _ ->
                        accum
            )
            []
        == [ "+", "-" ]

-}
fold : (Node Expression -> a -> a) -> a -> Node Expression -> a
fold function accum expr =
    foldHelper function accum [ expr ]


unParenthesize : Node Expression -> Node Expression
unParenthesize node =
    case Node.value node of
        ParenthesizedExpression exp ->
            unParenthesize exp

        _ ->
            node


normalizeApplicationHelper : Node Expression -> List (Node Expression) -> List (Node Expression)
normalizeApplicationHelper exp accum =
    case Node.value exp of
        Application (func :: args) ->
            normalizeApplicationHelper func (args ++ accum)

        OperatorApplication "<|" _ func arg ->
            normalizeApplicationHelper func (arg :: accum)

        OperatorApplication "|>" _ arg func ->
            normalizeApplicationHelper func (arg :: accum)

        ParenthesizedExpression innerExp ->
            normalizeApplicationHelper innerExp accum

        _ ->
            exp :: List.map unParenthesize accum


{-| Normalizes a function application expression for easier analysis. Nested function applications (including left and right pizza) will be unnested. Parenthesis around arguments expressions wlil be removed.

Assume that we have a function that can parse an Elm expression fragment `e : String -> Node Expression`, then the following statements are all true:

    normalizeApplication (e "a b") == [ e "a", e "b" ]

    normalizeApplication (e "a b c") == [ e "a", e "b", e "c" ]

    normalizeApplication (e "a <| b") == [ e "a", e "b" ]

    normalizeApplication (e "b |> a") == [ e "a", e "b" ]

    normalizeApplication (e "(a b) c)") == [ e "a", e "b", e "c" ]

    normalizeApplication (e "a (b)") == [ e "a", e "b" ]

-}
normalizeApplication : Node Expression -> List (Node Expression)
normalizeApplication exp =
    normalizeApplicationHelper exp []
