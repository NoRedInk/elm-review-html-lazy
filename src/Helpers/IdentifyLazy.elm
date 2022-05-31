module Helpers.IdentifyLazy exposing (importVisitor)

import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
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
