module UseMemoizedLambda exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Expression.Extra exposing (fold, normalizeApplication)
import Elm.Syntax.Node as Node exposing (Node(..))
import Helpers.IdentifyLazy as IdentifyLazy exposing (isLazyFunction)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (ContextCreator, Error, Rule)
import Set exposing (Set)


type alias ModuleContext =
    { importedNames : ModuleNameLookupTable
    , importedExposingAll : Set String
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseMemoizedLambda" initialContext
        |> Rule.withImportVisitor IdentifyLazy.importVisitor
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.fromModuleRuleSchema


initialContext : ContextCreator () ModuleContext
initialContext =
    Rule.initContextCreator
        (\importedNames _ ->
            { importedNames = importedNames
            , importedExposingAll = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


findLazyCalls : ModuleContext -> Node Expression -> List (Node Expression)
findLazyCalls moduleContext expression =
    fold
        (\exp accum ->
            if IdentifyLazy.isLazyFunction moduleContext exp then
                exp :: accum

            else
                accum
        )
        []
        expression
        |> List.reverse


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node moduleContext =
    case Node.value node of
        FunctionDeclaration { declaration } ->
            let
                decl =
                    Node.value declaration

                badLazyCalls =
                    case ( normalizeApplication decl.expression, decl.arguments ) of
                        ( [ lazyFunc, lambda ], [] ) ->
                            case ( isLazyFunction moduleContext lazyFunc, lambda ) of
                                ( True, Node _ (LambdaExpression _) ) ->
                                    findLazyCalls moduleContext lambda

                                _ ->
                                    findLazyCalls moduleContext decl.expression

                        _ ->
                            findLazyCalls moduleContext decl.expression

                errors =
                    badLazyCalls
                        |> List.map (Node.range >> Rule.error { message = "Calls to lazy should be memoized at the top level of a view function with a lambda function argument.", details = [ "See here" ] })
            in
            ( errors, moduleContext )

        _ ->
            ( [], moduleContext )
