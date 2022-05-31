module Helpers.IdentifyLazy exposing (importVisitor, isLazyFunction)

import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Error)
import Set exposing (Set)


{-| ModuleNameLookupTable works very well, unless a module is imported exposing all.
We use this Import Visitor to keep track of modules that are imported exposing everything.
-}
importVisitor :
    Node Import
    -> { context | importedExposingAll : Set String }
    -> ( List (Error {}), { context | importedExposingAll : Set String } )
importVisitor (Node _ { moduleName, exposingList }) context =
    case exposingList of
        Just (Node _ (All _)) ->
            ( [], { context | importedExposingAll = Set.insert (Node.value moduleName |> String.join ".") context.importedExposingAll } )

        _ ->
            ( [], context )


type alias KnownModule =
    { name : String
    , functions : Set String
    }


htmlLazyModule : KnownModule
htmlLazyModule =
    { name = "Html.Lazy"
    , functions = Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]
    }


htmlStyledLazyModule : KnownModule
htmlStyledLazyModule =
    { name = "Html.Styled.Lazy"
    , functions = Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7" ]
    }


allLazyFunctions : Set String
allLazyFunctions =
    Set.union htmlLazyModule.functions htmlStyledLazyModule.functions


isLazyFunction :
    { context | importedNames : ModuleNameLookupTable, importedExposingAll : Set String }
    -> Node Expression
    -> Bool
isLazyFunction { importedNames, importedExposingAll } node =
    case Node.value node of
        FunctionOrValue _ functionName ->
            Set.member functionName allLazyFunctions
                && (case ModuleNameLookupTable.moduleNameFor importedNames node of
                        Just ((_ :: _) as moduleNameList) ->
                            let
                                moduleName =
                                    moduleNameList |> String.join "."
                            in
                            moduleName == htmlLazyModule.name || moduleName == htmlStyledLazyModule.name

                        _ ->
                            -- This could happen if either `Html.Lazy` or `Html.Styled.Lazy` was imported exposing all and called unqualified
                            (Set.member htmlLazyModule.name importedExposingAll && Set.member functionName htmlLazyModule.functions)
                                || (Set.member htmlStyledLazyModule.name importedExposingAll && Set.member functionName htmlStyledLazyModule.functions)
                   )

        _ ->
            False
