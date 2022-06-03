module Elm.Syntax.Expression.Extra exposing (fold, normalizeApplication)

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


{-| Normalizes a function application expression for easier analysis.

    For example all the following expressions will be mapped to the same result of [a, b, c]

    a b c
    (a b) c
    a b <| c
    c |> a b

-}
normalizeApplication : Node Expression -> List (Node Expression)
normalizeApplication exp =
    normalizeApplicationHelper exp []
