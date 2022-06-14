module Helpers.IdentifyLazy exposing (identifyLazyFunction, importVisitor)

import Dict exposing (Dict)
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


airityTable : Dict String Int
airityTable =
    Dict.fromList
        [ ( "lazy", 1 )
        , ( "lazy2", 2 )
        , ( "lazy3", 3 )
        , ( "lazy4", 4 )
        , ( "lazy5", 5 )
        , ( "lazy6", 6 )
        , ( "lazy7", 7 )
        , ( "lazy8", 8 )
        ]


{-| Identifies a given node as a reference to lazy. If the node is a lazy call then the result of
`identifyLazyFunction` is a `Just` with the function name and the airity of the view function otherwise `Nothing`.

Note that actual airity of the lazy function is +1 as it takes the view function as an argument.

-}
identifyLazyFunction :
    { context | importedNames : ModuleNameLookupTable, importedExposingAll : Set String }
    -> Node Expression
    -> Maybe ( String, Int )
identifyLazyFunction { importedNames, importedExposingAll } node =
    case Node.value node of
        FunctionOrValue _ functionName ->
            case Dict.get functionName airityTable of
                Just airity ->
                    case ModuleNameLookupTable.moduleNameFor importedNames node of
                        Just ((_ :: _) as moduleNameList) ->
                            let
                                moduleName =
                                    moduleNameList |> String.join "."

                                isLazyModule =
                                    moduleName == htmlLazyModule.name || moduleName == htmlStyledLazyModule.name
                            in
                            if isLazyModule then
                                Just ( functionName, airity )

                            else
                                Nothing

                        _ ->
                            let
                                fromHtmlLazy =
                                    Set.member htmlLazyModule.name importedExposingAll && Set.member functionName htmlLazyModule.functions

                                fromHtmlStyledLazy =
                                    Set.member htmlStyledLazyModule.name importedExposingAll && Set.member functionName htmlStyledLazyModule.functions
                            in
                            if fromHtmlLazy || fromHtmlStyledLazy then
                                Just ( functionName, airity )

                            else
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
