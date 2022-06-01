module Elm.Syntax.Expression.Extra exposing (fold)

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))


fold : (Node Expression -> a -> a) -> a -> Node Expression -> a
fold function accum expr =
    let
        newAccum =
            function expr accum

        -- Reorder arguments for easy passing into List.foldl
        folder elem a =
            fold function a elem
    in
    case Node.value expr of
        Application exprs ->
            List.foldl folder newAccum exprs

        OperatorApplication _ _ leftExp rightExp ->
            List.foldl folder newAccum [ leftExp, rightExp ]

        IfBlock condExp trueExp falseExp ->
            List.foldl folder newAccum [ condExp, trueExp, falseExp ]

        Negation exp ->
            folder exp newAccum

        TupledExpression exps ->
            List.foldl folder newAccum exps

        ParenthesizedExpression exp ->
            folder exp newAccum

        LetExpression { declarations, expression } ->
            let
                mapLetDeclarations (Node _ letDeclaration) =
                    case letDeclaration of
                        LetFunction { declaration } ->
                            (Node.value declaration).expression

                        LetDestructuring _ exp ->
                            exp

                declAccum =
                    List.foldl (mapLetDeclarations >> folder) newAccum declarations
            in
            folder expression declAccum

        CaseExpression { expression, cases } ->
            List.foldl (Tuple.second >> folder) (folder expression newAccum) cases

        LambdaExpression { expression } ->
            folder expression newAccum

        RecordExpr recordSetters ->
            List.foldl (Node.value >> Tuple.second >> folder) newAccum recordSetters

        ListExpr exps ->
            List.foldl folder newAccum exps

        RecordAccess exp _ ->
            folder exp newAccum

        RecordUpdateExpression _ recordSetters ->
            List.foldl (Node.value >> Tuple.second >> folder) newAccum recordSetters

        _ ->
            newAccum
