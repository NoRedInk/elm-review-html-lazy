module Elm.Syntax.Denode exposing (..)

{-| This library provides a mirror of all types in `Elm.Syntax.*` modules without any `Node` wrappers and
    functions to `denode` from the former to the latter. 

    Why? Because `Node` wrapped values are extremely noisy when dumping to string with `Debug.toString`.  
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


type alias Comment =
    Comments.Comment


denodeComment : Comments.Comment -> Comment
denodeComment =
    identity



{- Elm.Syntax.Declaration -}


type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring Pattern Expression


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


type alias Documentation =
    Documentation.Documentation


denodeDocumentation : Documentation.Documentation -> Documentation
denodeDocumentation =
    identity



{- Elm.Syntax.Exposing -}


type Exposing
    = All
    | Explicit (List TopLevelExpose)


denodeExposing : Exposing.Exposing -> Exposing
denodeExposing x =
    case x of
        Exposing.All _ ->
            All

        Exposing.Explicit ls ->
            Explicit <| List.map (denodeTopLevelExpose << value) ls


type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


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


type alias ExposedType =
    { name : String
    , open : Maybe ()
    }


denodeExposedType : Exposing.ExposedType -> ExposedType
denodeExposedType { name, open } =
    { name = name
    , open = Maybe.map (\_ -> ()) open
    }



{-
   Elm.Syntax.Expression
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


type alias Lambda =
    { args : List Pattern
    , expression : Expression
    }


denodeLambda : Expression.Lambda -> Lambda
denodeLambda { args, expression } =
    { args = List.map (denodePattern << value) args
    , expression = expression |> value |> denodeExpression
    }


type alias LetBlock =
    { declarations : List LetDeclaration
    , expression : Expression
    }


denodeLetBlock : Expression.LetBlock -> LetBlock
denodeLetBlock { declarations, expression } =
    { declarations = List.map (denodeLetDeclaration << value) declarations
    , expression = expression |> value |> denodeExpression
    }


type LetDeclaration
    = LetFunction Function
    | LetDestructuring Pattern Expression


denodeLetDeclaration : Expression.LetDeclaration -> LetDeclaration
denodeLetDeclaration letDeclaration =
    case letDeclaration of
        Expression.LetFunction f ->
            LetFunction <| denodeFunction f

        Expression.LetDestructuring (Node _ p) (Node _ e) ->
            LetDestructuring (denodePattern p) (denodeExpression e)


type alias RecordSetter =
    ( String, Expression )


denodeRecordSetter : Expression.RecordSetter -> RecordSetter
denodeRecordSetter ( Node _ s, Node _ e ) =
    ( s, denodeExpression e )


type alias CaseBlock =
    { expression : Expression
    , cases : Cases
    }


denodeCaseBlock : Expression.CaseBlock -> CaseBlock
denodeCaseBlock { expression, cases } =
    { expression = expression |> value |> denodeExpression
    , cases = denodeCases cases
    }


type alias Cases =
    List Case


denodeCases : Expression.Cases -> Cases
denodeCases =
    List.map denodeCase


type alias Case =
    ( Pattern, Expression )


denodeCase : Expression.Case -> Case
denodeCase ( Node _ p, Node _ e ) =
    ( denodePattern p, denodeExpression e )


type alias Function =
    { documentation : Maybe Documentation
    , signature : Maybe Signature
    , declaration : FunctionImplementation
    }


denodeFunction : Expression.Function -> Function
denodeFunction { documentation, signature, declaration } =
    { documentation = Maybe.map (denodeDocumentation << value) documentation
    , signature = Maybe.map (denodeSignature << value) signature
    , declaration = declaration |> value |> denodeFunctionImplementation
    }


type alias FunctionImplementation =
    { name : String
    , arguments : List Pattern
    , expression : Expression
    }


denodeFunctionImplementation : Expression.FunctionImplementation -> FunctionImplementation
denodeFunctionImplementation { name, arguments, expression } =
    { name = name |> value
    , arguments = List.map (denodePattern << value) arguments
    , expression = expression |> value |> denodeExpression
    }



{- Elm.Syntax.File -}


type alias File =
    { moduleDefinition : Module
    , imports : List Import
    , declarations : List Declaration
    , comments : List Comment
    }


denodeFile : File.File -> File
denodeFile { moduleDefinition, imports, declarations, comments } =
    { moduleDefinition = moduleDefinition |> value |> denodeModule
    , imports = List.map (denodeImport << value) imports
    , declarations = List.map (denodeDeclaration << value) declarations
    , comments = List.map (denodeComment << value) comments
    }



{- import Elm.Syntax.Import as Import -}


type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Maybe Exposing
    }


denodeImport : Import.Import -> Import
denodeImport { moduleName, moduleAlias, exposingList } =
    { moduleName = moduleName |> value |> denodeModuleName
    , moduleAlias = moduleAlias |> Maybe.map (value >> denodeModuleName)
    , exposingList = exposingList |> Maybe.map (value >> denodeExposing)
    }



{- Elm.Syntax.Infix -}


type alias Infix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    , function : String
    }


denodeInfix : Infix.Infix -> Infix
denodeInfix { direction, precedence, operator, function } =
    { direction = direction |> value |> denodeInfixDirection
    , precedence = precedence |> value
    , operator = operator |> value
    , function = function |> value
    }


type alias InfixDirection =
    Infix.InfixDirection


denodeInfixDirection : Infix.InfixDirection -> InfixDirection
denodeInfixDirection =
    identity



{- Elm.Syntax.Module -}


type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


denodeModule : Module.Module -> Module
denodeModule m =
    case m of
        Module.NormalModule d ->
            NormalModule <| denodeDefaultModuleData d

        Module.PortModule d ->
            PortModule <| denodeDefaultModuleData d

        Module.EffectModule d ->
            EffectModule <| denodeEffectModuleData d


type alias DefaultModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    }


denodeDefaultModuleData : Module.DefaultModuleData -> DefaultModuleData
denodeDefaultModuleData { moduleName, exposingList } =
    { moduleName = moduleName |> value |> denodeModuleName
    , exposingList = exposingList |> value |> denodeExposing
    }


type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    , command : Maybe String
    , subscription : Maybe String
    }


denodeEffectModuleData : Module.EffectModuleData -> EffectModuleData
denodeEffectModuleData { moduleName, exposingList, command, subscription } =
    { moduleName = moduleName |> value |> denodeModuleName
    , exposingList = exposingList |> value |> denodeExposing
    , command = command |> Maybe.map value
    , subscription = subscription |> Maybe.map value
    }



{- Elm.Syntax.ModuleName -}


type alias ModuleName =
    ModuleName.ModuleName


denodeModuleName : ModuleName.ModuleName -> ModuleName
denodeModuleName =
    identity



{- Elm.Syntax.Pattern -}


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


type alias QualifiedNameRef =
    Pattern.QualifiedNameRef


denodeQualifiedNameRef : Pattern.QualifiedNameRef -> QualifiedNameRef
denodeQualifiedNameRef =
    identity



{-
   Elm.Syntax.Signature
-}


type alias Signature =
    { name : String
    , typeAnnotation : TypeAnnotation
    }


denodeSignature : Signature.Signature -> Signature
denodeSignature { name, typeAnnotation } =
    { name = name |> value
    , typeAnnotation = typeAnnotation |> value |> denodeTypeAnnotation
    }



{- Elm.Syntax.Type -}


type alias Type =
    { documentation : Maybe Documentation
    , name : String
    , generics : List String
    , constructors : List ValueConstructor
    }


denodeType : Type.Type -> Type
denodeType { documentation, name, generics, constructors } =
    { documentation = documentation |> Maybe.map (value >> denodeDocumentation)
    , name = name |> value
    , generics = generics |> List.map value
    , constructors = constructors |> List.map (value >> denodeValueConstructor)
    }


type alias ValueConstructor =
    { name : String
    , arguments : List TypeAnnotation
    }


denodeValueConstructor : Type.ValueConstructor -> ValueConstructor
denodeValueConstructor { name, arguments } =
    { name = name |> value
    , arguments = arguments |> List.map (value >> denodeTypeAnnotation)
    }



{- Elm.Syntax.TypeAlias -}


type alias TypeAlias =
    { documentation : Maybe Documentation
    , name : String
    , generics : List String
    , typeAnnotation : TypeAnnotation
    }


denodeTypeAlias : TypeAlias.TypeAlias -> TypeAlias
denodeTypeAlias { documentation, name, generics, typeAnnotation } =
    { documentation = documentation |> Maybe.map (value >> denodeDocumentation)
    , name = name |> value
    , generics = generics |> List.map value
    , typeAnnotation = typeAnnotation |> value |> denodeTypeAnnotation
    }



{- Elm.Syntax.TypeAnnotation -}


type TypeAnnotation
    = GenericType String
    | Typed ( ModuleName, String ) (List TypeAnnotation)
    | Unit
    | Tupled (List TypeAnnotation)
    | Record RecordDefinition
    | GenericRecord String RecordDefinition
    | FunctionTypeAnnotation TypeAnnotation TypeAnnotation


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


type alias RecordDefinition =
    List RecordField


denodeRecordDefinition : TypeAnnotation.RecordDefinition -> RecordDefinition
denodeRecordDefinition =
    List.map (value >> denodeRecordField)


type alias RecordField =
    ( String, TypeAnnotation )


denodeRecordField : TypeAnnotation.RecordField -> RecordField
denodeRecordField ( Node _ s, Node _ ta ) =
    ( s, denodeTypeAnnotation ta )
