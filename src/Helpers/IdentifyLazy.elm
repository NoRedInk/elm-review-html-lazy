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


knownModules : List KnownModule
knownModules =
    [ { name = "Html.Lazy"
      , functions = lazySet 8
      }
    , { name = "Html.Styled.Lazy"
      , functions = lazySet 7
      }
    , { name = "Element.Lazy"
      , functions = lazySet 5
      }
    , { name = "Element.WithContext.Lazy"
      , functions = lazySet 3
      }
    ]


lazySet : Int -> Set String
lazySet n =
    lazySetHelper n Set.empty


lazySetHelper : Int -> Set String -> Set String
lazySetHelper n acc =
    if n > 1 then
        lazySetHelper
            (n - 1)
            (Set.insert ("lazy" ++ String.fromInt n) acc)

    else
        Set.insert "lazy" acc


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
                                moduleName : String
                                moduleName =
                                    moduleNameList |> String.join "."

                                isLazyModule : Bool
                                isLazyModule =
                                    knownModules
                                        |> List.map .name
                                        |> List.member moduleName
                            in
                            if isLazyModule then
                                Just ( functionName, airity )

                            else
                                Nothing

                        _ ->
                            let
                                fromKnown : Bool
                                fromKnown =
                                    List.any
                                        (\mod ->
                                            Set.member mod.name importedExposingAll
                                                && Set.member functionName mod.functions
                                        )
                                        knownModules
                            in
                            if fromKnown then
                                Just ( functionName, airity )

                            else
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
