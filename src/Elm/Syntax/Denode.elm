module Elm.Syntax.Denode exposing
    ( Comment
    , Declaration(..)
    , Documentation
    , Exposing(..), TopLevelExpose(..), ExposedType
    , Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation
    , File, denodeFile
    , Import
    , Infix, InfixDirection
    , Module(..), DefaultModuleData, EffectModuleData
    , Pattern(..), QualifiedNameRef
    , Signature
    , Type, ValueConstructor
    , TypeAlias
    , RecordDefinition, RecordField
    , ModuleName, TypeAnnotation(..)
    )

{-| This library provides a mirror of all types in `Elm.Syntax.*` modules without any `Node` wrappers and
functions to `denode` from the former to the latter.

Why? Because `Node` wrapped values are extremely noisy when dumping to string with `Debug.toString`. Compare the output of the following expressions

    e "1 + 2"
        |> Debug.toString

Yields

    Node
        { end = { column = 13, row = 4 }
        , start = { column = 8, row = 4 }
        }
        (OperatorApplication "+"
            Left
            (Node
                { end = { column = 9, row = 4 }
                , start = { column = 8, row = 4 }
                }
                (Integer 1)
            )
            (Node
                { end = { column = 13, row = 4 }
                , start = { column = 12, row = 4 }
                }
                (Integer 2)
            )
        )

And when using `denodeExpression`:

    e "1 + 2"
        |> Node.value
        |> denodeExpression
        |> Debug.toString

Yields

    OperatorApplication "+" Left (Integer 1) (Integer 2)


## Elm.Syntax.Comments

@docs Comment


## Elm.Syntax.Declaration

@docs Declaration


## Elm.Syntax.Documentation

@docs Documentation


## Elm.Syntax.Exposing

@docs Exposing, TopLevelExpose, ExposedType


## Elm.Syntax.Expression

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation


## Elm.Syntax.File

@docs File, denodeFile


## Elm.Syntax.Import

@docs Import


## Elm.Syntax.Infix

@docs Infix, InfixDirection


## Elm.Syntax.Module

@docs Module, denodeModule, DefaultModuleData, EffectModuleData


## Elm.Syntax.ModuleName

@docs ModuleNameName


## Elm.Syntax.Pattern

@docs Pattern, QualifiedNameRef


## Elm.Syntax.Signature

@docs Signature


## Elm.Syntax.Type

@docs Type, denodeType, ValueConstructor


## Elm.Syntax.TypeAlias

@docs TypeAlias


## Elm.Syntax.TypeAnnotation

@docs TypeAnnotationAnnotation, RecordDefinition, RecordField

-}

import Elm.Syntax.Comments as Comments
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Documentation as Documentation
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Import as Import
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName as ModuleName
import Elm.Syntax.Node exposing (Node(..), value)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Signature as Signature
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAlias as TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)



{- Elm.Syntax.Comments -}


{-| Mirrors `Elm.Syntax.Comments.Comment`
-}
type alias Comment =
    Comments.Comment


{-| Remove all node information from a `Comment`
-}
denodeComment : Comments.Comment -> Comment
denodeComment =
    identity



{- Elm.Syntax.Declaration -}


{-| Mirrors `Declaration` from [Elm.Syntax.Declaration](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring Pattern Expression


{-| Remove all node information from a `Declaration`
-}
denodeDeclaration : Declaration.Declaration -> Declaration
denodeDeclaration x =
    case x of
        Declaration.FunctionDeclaration f ->
            FunctionDeclaration (denodeFunction f)

        Declaration.AliasDeclaration t ->
            AliasDeclaration (denodeTypeAlias t)

        Declaration.CustomTypeDeclaration t ->
            CustomTypeDeclaration (denodeType t)

        Declaration.PortDeclaration s ->
            PortDeclaration (denodeSignature s)

        Declaration.InfixDeclaration i ->
            InfixDeclaration (denodeInfix i)

        Declaration.Destructuring (Node _ p) (Node _ e) ->
            Destructuring (denodePattern p) (denodeExpression e)



{- Elm.Syntax.Documentation -}


{-| Mirrors `Documentation` from [Elm.Syntax.Documentation](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Documentation)
-}
type alias Documentation =
    Documentation.Documentation


{-| Remove all node information from a `Documentation`
-}
denodeDocumentation : Documentation.Documentation -> Documentation
denodeDocumentation =
    identity



{- Elm.Syntax.Exposing -}


{-| Mirrors `Elm.Syntax.Exposing.Exposing`
-}
type Exposing
    = All
    | Explicit (List TopLevelExpose)


{-| Remove all node information from a `Exposing`
-}
denodeExposing : Exposing.Exposing -> Exposing
denodeExposing x =
    case x of
        Exposing.All _ ->
            All

        Exposing.Explicit ls ->
            Explicit <| List.map (denodeTopLevelExpose << value) ls


{-| Mirrors `Elm.Syntax.Exposing.TopLevelExpose`
-}
type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


{-| Remove all node information from a `TopLevelExpose`
-}
denodeTopLevelExpose : Exposing.TopLevelExpose -> TopLevelExpose
denodeTopLevelExpose topLevelExpose =
    case topLevelExpose of
        Exposing.InfixExpose s ->
            InfixExpose <| s

        Exposing.FunctionExpose s ->
            FunctionExpose <| s

        Exposing.TypeOrAliasExpose s ->
            TypeOrAliasExpose <| s

        Exposing.TypeExpose e ->
            TypeExpose <| denodeExposedType e


{-| Mirrors `Elm.Syntax.Exposing.ExposedType`
-}
type alias ExposedType =
    { name : String
    , open : Maybe ()
    }


{-| Remove all node information from a `ExposedType`
-}
denodeExposedType : Exposing.ExposedType -> ExposedType
denodeExposedType { name, open } =
    { name = name
    , open = Maybe.map (\_ -> ()) open
    }



{-
   Elm.Syntax.Expression
-}


{-| Mirros `Elm.Syntax.Expression.Expression`
-}
type Expression
    = UnitExpr
    | Application (List Expression)
    | OperatorApplication String InfixDirection Expression Expression
    | FunctionOrValue ModuleName String
    | IfBlock Expression Expression Expression
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation Expression
    | Literal String
    | CharLiteral Char
    | TupledExpression (List Expression)
    | ParenthesizedExpression Expression
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List RecordSetter)
    | ListExpr (List Expression)
    | RecordAccess Expression String
    | RecordAccessFunction String
    | RecordUpdateExpression String (List RecordSetter)
    | GLSLExpression String


{-| Removes all node information from an `Expression`
-}
denodeExpression : Expression.Expression -> Expression
denodeExpression expression =
    case expression of
        Expression.UnitExpr ->
            UnitExpr

        Expression.Application es ->
            Application <| List.map (denodeExpression << value) es

        Expression.OperatorApplication op id (Node _ el) (Node _ er) ->
            OperatorApplication op (denodeInfixDirection id) (denodeExpression el) (denodeExpression er)

        Expression.FunctionOrValue mn n ->
            FunctionOrValue mn n

        Expression.IfBlock (Node _ co) (Node _ l) (Node _ r) ->
            IfBlock (denodeExpression co) (denodeExpression l) (denodeExpression r)

        Expression.PrefixOperator s ->
            PrefixOperator s

        Expression.Operator s ->
            Operator s

        Expression.Integer i ->
            Integer i

        Expression.Hex i ->
            Hex i

        Expression.Floatable f ->
            Floatable f

        Expression.Negation (Node _ e) ->
            Negation <| denodeExpression e

        Expression.Literal s ->
            Literal s

        Expression.CharLiteral c ->
            CharLiteral c

        Expression.TupledExpression es ->
            TupledExpression <| List.map (denodeExpression << value) es

        Expression.ParenthesizedExpression (Node _ e) ->
            ParenthesizedExpression <| denodeExpression e

        Expression.LetExpression l ->
            LetExpression <| denodeLetBlock l

        Expression.CaseExpression c ->
            CaseExpression <| denodeCaseBlock c

        Expression.LambdaExpression l ->
            LambdaExpression <| denodeLambda l

        Expression.RecordExpr rs ->
            RecordExpr <| List.map (denodeRecordSetter << value) rs

        Expression.ListExpr es ->
            ListExpr <| List.map (denodeExpression << value) es

        Expression.RecordAccess (Node _ e) (Node _ s) ->
            RecordAccess (denodeExpression e) s

        Expression.RecordAccessFunction s ->
            RecordAccessFunction s

        Expression.RecordUpdateExpression (Node _ s) rs ->
            RecordUpdateExpression s <| List.map (denodeRecordSetter << value) rs

        Expression.GLSLExpression s ->
            GLSLExpression s


{-| Mirrors `Elm.Syntax.Expression.Lambda`
-}
type alias Lambda =
    { args : List Pattern
    , expression : Expression
    }


{-| Removes all node information from a `Lambda`
-}
denodeLambda : Expression.Lambda -> Lambda
denodeLambda { args, expression } =
    { args = List.map (denodePattern << value) args
    , expression = expression |> value |> denodeExpression
    }


{-| Mirrors `Elm.Syntax.Expression.LetBlock`
-}
type alias LetBlock =
    { declarations : List LetDeclaration
    , expression : Expression
    }


{-| Removes all node information from a `LetBlock`
-}
denodeLetBlock : Expression.LetBlock -> LetBlock
denodeLetBlock { declarations, expression } =
    { declarations = List.map (denodeLetDeclaration << value) declarations
    , expression = expression |> value |> denodeExpression
    }


{-| Mirrors `Elm.Syntax.Expression.LetDeclaration`
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring Pattern Expression


{-| Removes all node information from a `LetDeclaration`
-}
denodeLetDeclaration : Expression.LetDeclaration -> LetDeclaration
denodeLetDeclaration letDeclaration =
    case letDeclaration of
        Expression.LetFunction f ->
            LetFunction <| denodeFunction f

        Expression.LetDestructuring (Node _ p) (Node _ e) ->
            LetDestructuring (denodePattern p) (denodeExpression e)


{-| Mirrors `Elm.Syntax.Expression.RecordSetter`
-}
type alias RecordSetter =
    ( String, Expression )


{-| Removes all node information from a `RecordSetter`
-}
denodeRecordSetter : Expression.RecordSetter -> RecordSetter
denodeRecordSetter ( Node _ s, Node _ e ) =
    ( s, denodeExpression e )


{-| Mirrors `Elm.Syntax.Expression.CaseBlock`
-}
type alias CaseBlock =
    { expression : Expression
    , cases : Cases
    }


{-| Removes all node information from a `CaseBlock`
-}
denodeCaseBlock : Expression.CaseBlock -> CaseBlock
denodeCaseBlock { expression, cases } =
    { expression = expression |> value |> denodeExpression
    , cases = denodeCases cases
    }


{-| Mirrors `Elm.Syntax.Expression.Cases`
-}
type alias Cases =
    List Case


{-| Removes all node information from a `Cases`
-}
denodeCases : Expression.Cases -> Cases
denodeCases =
    List.map denodeCase


{-| Mirrors `Elm.Syntax.Expression.Case`
-}
type alias Case =
    ( Pattern, Expression )


{-| Removes all node information from a `Case`
-}
denodeCase : Expression.Case -> Case
denodeCase ( Node _ p, Node _ e ) =
    ( denodePattern p, denodeExpression e )


{-| Mirrors `Elm.Syntax.Expression.Function`
-}
type alias Function =
    { documentation : Maybe Documentation
    , signature : Maybe Signature
    , declaration : FunctionImplementation
    }


{-| Removes all node information from a `Function`
-}
denodeFunction : Expression.Function -> Function
denodeFunction { documentation, signature, declaration } =
    { documentation = Maybe.map (denodeDocumentation << value) documentation
    , signature = Maybe.map (denodeSignature << value) signature
    , declaration = declaration |> value |> denodeFunctionImplementation
    }


{-| Mirrors `Elm.Syntax.Expression.FunctionImplementation`
-}
type alias FunctionImplementation =
    { name : String
    , arguments : List Pattern
    , expression : Expression
    }


{-| Removes all node information from a `FunctionImplementation`
-}
denodeFunctionImplementation : Expression.FunctionImplementation -> FunctionImplementation
denodeFunctionImplementation { name, arguments, expression } =
    { name = name |> value
    , arguments = List.map (denodePattern << value) arguments
    , expression = expression |> value |> denodeExpression
    }



{- Elm.Syntax.File -}


{-| Mirrors `Elm.Syntax.File.File`
-}
type alias File =
    { moduleDefinition : Module
    , imports : List Import
    , declarations : List Declaration
    , comments : List Comment
    }


{-| Removes all node information from a `File`
-}
denodeFile : File.File -> File
denodeFile { moduleDefinition, imports, declarations, comments } =
    { moduleDefinition = moduleDefinition |> value |> denodeModule
    , imports = List.map (denodeImport << value) imports
    , declarations = List.map (denodeDeclaration << value) declarations
    , comments = List.map (denodeComment << value) comments
    }



{- import Elm.Syntax.Import as Import -}


{-| Mirrors `Elm.Syntax.Import.Import`
-}
type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Maybe Exposing
    }


{-| Removes all node information from an `Import`
-}
denodeImport : Import.Import -> Import
denodeImport { moduleName, moduleAlias, exposingList } =
    { moduleName = moduleName |> value |> denodeModuleName
    , moduleAlias = moduleAlias |> Maybe.map (value >> denodeModuleName)
    , exposingList = exposingList |> Maybe.map (value >> denodeExposing)
    }



{- Elm.Syntax.Infix -}


{-| Mirrors `Elm.Syntax.Infix.Infix`
-}
type alias Infix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    , function : String
    }


{-| Removes all node information from an `Infix`
-}
denodeInfix : Infix.Infix -> Infix
denodeInfix { direction, precedence, operator, function } =
    { direction = direction |> value |> denodeInfixDirection
    , precedence = precedence |> value
    , operator = operator |> value
    , function = function |> value
    }


{-| Mirrors `Elm.Syntax.Infix.InfixDirection`
-}
type alias InfixDirection =
    Infix.InfixDirection


{-| Removes all node information from an `InfixDirection`
-}
denodeInfixDirection : Infix.InfixDirection -> InfixDirection
denodeInfixDirection =
    identity



{- Elm.Syntax.Module -}


{-| Mirrors `Elm.Syntax.Module.Module`
-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Removes all node information from a `Module`
-}
denodeModule : Module.Module -> Module
denodeModule m =
    case m of
        Module.NormalModule d ->
            NormalModule <| denodeDefaultModuleData d

        Module.PortModule d ->
            PortModule <| denodeDefaultModuleData d

        Module.EffectModule d ->
            EffectModule <| denodeEffectModuleData d


{-| Mirrors `Elm.Syntax.Module.DefaultModuleData`
-}
type alias DefaultModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    }


{-| Removes all node information from a `DefaultModuleData`
-}
denodeDefaultModuleData : Module.DefaultModuleData -> DefaultModuleData
denodeDefaultModuleData { moduleName, exposingList } =
    { moduleName = moduleName |> value |> denodeModuleName
    , exposingList = exposingList |> value |> denodeExposing
    }


{-| Mirrors `Elm.Syntax.Module.EffectModuleData`
-}
type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    , command : Maybe String
    , subscription : Maybe String
    }


{-| Removes all node information from an `EffectModuleData`
-}
denodeEffectModuleData : Module.EffectModuleData -> EffectModuleData
denodeEffectModuleData { moduleName, exposingList, command, subscription } =
    { moduleName = moduleName |> value |> denodeModuleName
    , exposingList = exposingList |> value |> denodeExposing
    , command = command |> Maybe.map value
    , subscription = subscription |> Maybe.map value
    }



{- Elm.Syntax.ModuleName -}


{-| Mirrors `Elm.Syntax.ModuleName.ModuleName`
-}
type alias ModuleName =
    ModuleName.ModuleName


{-| Removes all node information from a `ModuleName`
-}
denodeModuleName : ModuleName.ModuleName -> ModuleName
denodeModuleName =
    identity



{- Elm.Syntax.Pattern -}


{-| Mirrors `Elm.Syntax.Pattern.Pattern`
-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List Pattern)
    | RecordPattern (List String)
    | UnConsPattern Pattern Pattern
    | ListPattern (List Pattern)
    | VarPattern String
    | NamedPattern QualifiedNameRef (List Pattern)
    | AsPattern Pattern String
    | ParenthesizedPattern Pattern


{-| Removes all node information from a `Pattern`
-}
denodePattern : Pattern.Pattern -> Pattern
denodePattern pattern =
    case pattern of
        Pattern.AllPattern ->
            AllPattern

        Pattern.UnitPattern ->
            UnitPattern

        Pattern.CharPattern c ->
            CharPattern c

        Pattern.StringPattern s ->
            StringPattern s

        Pattern.IntPattern i ->
            IntPattern i

        Pattern.HexPattern i ->
            HexPattern i

        Pattern.FloatPattern f ->
            FloatPattern f

        Pattern.TuplePattern ts ->
            TuplePattern <| List.map (value >> denodePattern) ts

        Pattern.RecordPattern rs ->
            RecordPattern <| List.map value rs

        Pattern.UnConsPattern (Node _ h) (Node _ t) ->
            UnConsPattern (denodePattern h) (denodePattern t)

        Pattern.ListPattern ls ->
            ListPattern <| List.map (value >> denodePattern) ls

        Pattern.VarPattern s ->
            VarPattern s

        Pattern.NamedPattern qn ps ->
            NamedPattern (denodeQualifiedNameRef qn) <| List.map (value >> denodePattern) ps

        Pattern.AsPattern (Node _ p) (Node _ s) ->
            AsPattern (denodePattern p) s

        Pattern.ParenthesizedPattern (Node _ p) ->
            ParenthesizedPattern <| denodePattern p


{-| Mirrors `Elm.Syntax.Pattern.QualifiedNameRef`
-}
type alias QualifiedNameRef =
    Pattern.QualifiedNameRef


{-| Removes all node information from a `QualifiedNameRef`
-}
denodeQualifiedNameRef : Pattern.QualifiedNameRef -> QualifiedNameRef
denodeQualifiedNameRef =
    identity



{-
   Elm.Syntax.Signature
-}


{-| Mirrors `Elm.Syntax.Signature.Signature`
-}
type alias Signature =
    { name : String
    , typeAnnotation : TypeAnnotation
    }


{-| Removes all node information from a `Signature`
-}
denodeSignature : Signature.Signature -> Signature
denodeSignature { name, typeAnnotation } =
    { name = name |> value
    , typeAnnotation = typeAnnotation |> value |> denodeTypeAnnotation
    }



{- Elm.Syntax.Type -}


{-| Mirrors `Elm.Syntax.Type.Type`
-}
type alias Type =
    { documentation : Maybe Documentation
    , name : String
    , generics : List String
    , constructors : List ValueConstructor
    }


{-| Removes all node information from a `Type`
-}
denodeType : Type.Type -> Type
denodeType { documentation, name, generics, constructors } =
    { documentation = documentation |> Maybe.map (value >> denodeDocumentation)
    , name = name |> value
    , generics = generics |> List.map value
    , constructors = constructors |> List.map (value >> denodeValueConstructor)
    }


{-| Mirrors `Elm.Syntax.Type.ValueConstructor`
-}
type alias ValueConstructor =
    { name : String
    , arguments : List TypeAnnotation
    }


{-| Removes all node information from a `ValueConstructor`
-}
denodeValueConstructor : Type.ValueConstructor -> ValueConstructor
denodeValueConstructor { name, arguments } =
    { name = name |> value
    , arguments = arguments |> List.map (value >> denodeTypeAnnotation)
    }



{- Elm.Syntax.TypeAlias -}


{-| Mirrors `Elm.Syntax.TypeAlias.TypeAlias`
-}
type alias TypeAlias =
    { documentation : Maybe Documentation
    , name : String
    , generics : List String
    , typeAnnotation : TypeAnnotation
    }


{-| Removes all node information from a `TypeAlias`
-}
denodeTypeAlias : TypeAlias.TypeAlias -> TypeAlias
denodeTypeAlias { documentation, name, generics, typeAnnotation } =
    { documentation = documentation |> Maybe.map (value >> denodeDocumentation)
    , name = name |> value
    , generics = generics |> List.map value
    , typeAnnotation = typeAnnotation |> value |> denodeTypeAnnotation
    }



{- Elm.Syntax.TypeAnnotation -}


{-| Mirrors `Elm.Syntax.TypeAnnotation.TypeAnnotation`
-}
type TypeAnnotation
    = GenericType String
    | Typed ( ModuleName, String ) (List TypeAnnotation)
    | Unit
    | Tupled (List TypeAnnotation)
    | Record RecordDefinition
    | GenericRecord String RecordDefinition
    | FunctionTypeAnnotation TypeAnnotation TypeAnnotation


{-| Removes all node information from a `TypeAnnotation`
-}
denodeTypeAnnotation : TypeAnnotation.TypeAnnotation -> TypeAnnotation
denodeTypeAnnotation typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.GenericType s ->
            GenericType s

        TypeAnnotation.Typed (Node _ ( mn, s )) ts ->
            Typed ( denodeModuleName mn, s ) <| List.map (value >> denodeTypeAnnotation) ts

        TypeAnnotation.Unit ->
            Unit

        TypeAnnotation.Tupled ts ->
            Tupled <| List.map (value >> denodeTypeAnnotation) ts

        TypeAnnotation.Record r ->
            Record <| denodeRecordDefinition r

        TypeAnnotation.GenericRecord (Node _ s) (Node _ rd) ->
            GenericRecord s <| denodeRecordDefinition rd

        TypeAnnotation.FunctionTypeAnnotation (Node _ lt) (Node _ rt) ->
            FunctionTypeAnnotation (denodeTypeAnnotation lt) (denodeTypeAnnotation rt)


{-| Mirrors `Elm.Syntax.TypeAnnotation.RecordDefinition`
-}
type alias RecordDefinition =
    List RecordField


{-| Removes all type information from a `RecordDefinition`
-}
denodeRecordDefinition : TypeAnnotation.RecordDefinition -> RecordDefinition
denodeRecordDefinition =
    List.map (value >> denodeRecordField)


{-| Mirrors `Elm.Syntax.TypeAnnotation.RecordField`
-}
type alias RecordField =
    ( String, TypeAnnotation )


{-| Removes all type information from a `RecordField`
-}
denodeRecordField : TypeAnnotation.RecordField -> RecordField
denodeRecordField ( Node _ s, Node _ ta ) =
    ( s, denodeTypeAnnotation ta )
