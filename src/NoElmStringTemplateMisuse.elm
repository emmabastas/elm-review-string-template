module NoElmStringTemplateMisuse exposing (rule)

{-|

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Regex exposing (Regex)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ NoElmStringTemplateMisuse.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
THis rule is not useful when REPLACEME.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoElmStringTemplateMisuse"
        { injectQualified = Nothing
        , injectUnqualified = False
        }
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { injectQualified : Maybe ( List String, String )
    , injectUnqualified : Bool
    }


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor (Node _ import_) context =
    case import_.moduleName of
        Node _ [ "String", "Template" ] ->
            let
                injectQualified =
                    case import_.moduleAlias of
                        Nothing ->
                            Just ( [ "String", "Template" ], "inject" )

                        Just (Node _ qualifier) ->
                            Just ( qualifier, "inject" )

                injectUnqualified =
                    case import_.exposingList of
                        Nothing ->
                            False

                        Just (Node _ (Exposing.All _)) ->
                            True

                        Just (Node _ (Exposing.Explicit exposings)) ->
                            List.any
                                (\(Node _ exp) ->
                                    case exp of
                                        Exposing.FunctionExpose "inject" ->
                                            True

                                        _ ->
                                            False
                                )
                                exposings
            in
            ( []
            , { injectQualified = injectQualified
              , injectUnqualified = injectUnqualified
              }
            )

        _ ->
            ( [], context )


expressionVisitor :
    Node Expression
    -> Rule.Direction
    -> Context
    -> ( List (Error {}), Context )
expressionVisitor (Node _ expr) direction context =
    case extractTemplateInjectApplication direction context expr of
        Just ( Node _ (ListExpr keyExprs), Node literalRange (Literal template) ) ->
            let
                keys : List (Node String)
                keys =
                    List.filterMap
                        (\keyExpr ->
                            case Node.value keyExpr of
                                Expression.TupledExpression [ Node range (Expression.Literal key), _ ] ->
                                    Just (Node range key)

                                _ ->
                                    Nothing
                        )
                        keyExprs

                templateRange : Range
                templateRange =
                    stringRangeInLiteral (Node literalRange template)
            in
            if List.length keyExprs == List.length keys then
                ( checkForErrors (Node templateRange template) keys
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )


extractTemplateInjectApplication :
    Rule.Direction
    -> Context
    -> Expression
    -> Maybe ( Node Expression, Node Expression )
extractTemplateInjectApplication direction context expr =
    case ( direction, context.injectQualified, context.injectUnqualified ) of
        ( Rule.OnExit, _, _ ) ->
            Nothing

        ( Rule.OnEnter, Nothing, False ) ->
            Nothing

        ( Rule.OnEnter, _, _ ) ->
            case expr of
                Application [ Node _ (FunctionOrValue mn n), e1, e2 ] ->
                    if isStringTemplateInject context mn n then
                        Just ( e1, e2 )

                    else
                        Nothing

                OperatorApplication "|>" _ e1 (Node _ (Application [ Node _ (FunctionOrValue mn n), e2 ])) ->
                    if isStringTemplateInject context mn n then
                        Just ( e2, e1 )

                    else
                        Nothing

                OperatorApplication "<|" _ (Node _ (Application [ Node _ (FunctionOrValue mn n), e1 ])) e2 ->
                    if isStringTemplateInject context mn n then
                        Just ( e1, e2 )

                    else
                        Nothing

                _ ->
                    Nothing


isStringTemplateInject : Context -> List String -> String -> Bool
isStringTemplateInject { injectQualified, injectUnqualified } moduleName name =
    [ case injectQualified of
        Nothing ->
            False

        Just ( mn, n ) ->
            mn == moduleName && n == name
    , injectUnqualified && moduleName == [] && name == "inject"
    ]
        |> List.any identity


checkForErrors : Node String -> List (Node String) -> List (Error {})
checkForErrors template keyNodes =
    let
        ( errors, dict ) =
            keyNodesToDict keyNodes
    in
    veryifyTemplate dict template ++ errors


keyNodesToDict : List (Node String) -> ( List (Error {}), Dict String Range )
keyNodesToDict keyNodes =
    List.foldl
        (\(Node range key) ( errs, d ) ->
            case Dict.get key d of
                Just _ ->
                    ( duplicateKeysError range :: errs, d )

                Nothing ->
                    ( errs, Dict.insert key range d )
        )
        ( [], Dict.empty )
        keyNodes


veryifyTemplate : Dict String Range -> Node String -> List (Error {})
veryifyTemplate dict template =
    let
        ( placeholdersWithoutValue, unusedKeys ) =
            List.foldl
                (\placeholder ( ps, uk ) ->
                    case Dict.get placeholder.name dict of
                        Just _ ->
                            ( ps, Dict.remove placeholder.name uk )

                        Nothing ->
                            ( placeholder :: ps, uk )
                )
                ( [], dict )
                (placeholdersInTemplate (Node.value template))

        multilineString =
            (Node.range template).start.row /= (Node.range template).end.row

        templateCursorPosition : Array ( Int, Int )
        templateCursorPosition =
            List.foldl
                (\char ( ( c, r ), xs ) ->
                    case char of
                        '\n' ->
                            if multilineString then
                                ( ( 1, r + 1 ), ( 1, r + 1 ) :: xs )

                            else
                                ( ( c + 2, r ), ( c + 2, r ) :: xs )

                        _ ->
                            ( ( c + 1, r ), ( c + 1, r ) :: xs )
                )
                ( ( 0, 1 ), [] )
                (String.toList (Node.value template))
                |> Tuple.second
                |> List.reverse
                |> Array.fromList

        placeholderWithoutValueErrors : List (Error {})
        placeholderWithoutValueErrors =
            List.map
                (\{ startIndex, endIndex } ->
                    let
                        ( sc, sr ) =
                            Array.get startIndex templateCursorPosition
                                |> Maybe.withDefault ( 0, 0 )

                        ( ec, er ) =
                            Array.get (endIndex - 1) templateCursorPosition
                                |> Maybe.withDefault ( 0, 0 )

                        range =
                            offsetRange
                                (Node.range template).start
                                { start = Location sr sc
                                , end = Location er ec
                                }
                    in
                    placeholderWithoutValueError range
                )
                placeholdersWithoutValue

        unusedKeyErrors : List (Error {})
        unusedKeyErrors =
            List.map
                (\( _, range ) -> unusedKeyError range)
                (Dict.toList unusedKeys)
    in
    placeholderWithoutValueErrors ++ unusedKeyErrors


placeholdersInTemplate : String -> List { name : String, startIndex : Int, endIndex : Int }
placeholdersInTemplate template =
    let
        regex : Regex
        regex =
            Regex.fromString "\\${[^}]*}"
                |> Maybe.withDefault Regex.never
    in
    List.map
        (\{ match, index } ->
            { name = nameInPlaceholder match
            , startIndex = index
            , endIndex = index + String.length match
            }
        )
        (Regex.find regex template)


nameInPlaceholder : String -> String
nameInPlaceholder placeholder =
    placeholder
        |> String.dropLeft 2
        |> String.dropRight 1


stringRangeInLiteral : Node String -> Range
stringRangeInLiteral (Node range string) =
    if range.start.row == range.end.row then
        let
            literalLength =
                range.end.column - range.start.column

            stringLength =
                String.length string

            delta =
                (literalLength - stringLength) // 2
        in
        { start =
            { row = range.start.row
            , column = range.start.column + delta
            }
        , end =
            { row = range.end.row
            , column = range.end.column - delta
            }
        }

    else
        let
            stringRange =
                rangeFromString string

            delta =
                range.end.column - stringRange.end.column
        in
        { start =
            { row = range.start.row
            , column = range.start.column + delta
            }
        , end =
            { row = range.end.row
            , column = range.end.column - delta
            }
        }


rangeFromString : String -> Range
rangeFromString string =
    let
        lines =
            String.lines string

        lastLine =
            List.head (List.reverse lines)
                |> Maybe.withDefault ""
    in
    { start =
        { row = 1
        , column = 1
        }
    , end =
        { row = List.length lines
        , column = String.length lastLine + 1
        }
    }


offsetRange : Location -> Range -> Range
offsetRange { row, column } { start, end } =
    { start =
        { row = row + start.row - 1
        , column =
            if start.row == 1 then
                column + start.column - 1

            else
                start.column - 1
        }
    , end =
        { row = row + end.row - 1
        , column =
            if end.row == 1 then
                column + end.column

            else
                end.column
        }
    }



-- Review errors


duplicateKeysError : Range -> Error {}
duplicateKeysError =
    Rule.error
        { message = "Duplicate keys."
        , details = [ "You already have a key with the same name. Rename this key to something eles" ]
        }


unusedKeyError : Range -> Error {}
unusedKeyError =
    Rule.error
        { message = "Unused key."
        , details = [ "This keys is not being used anywhere in the template, maybe you meant to use it but misspelled the key or placeholder name?" ]
        }


placeholderWithoutValueError : Range -> Error {}
placeholderWithoutValueError =
    Rule.error
        { message = "Placeholder has no value"
        , details = [ "This placeholder has no value associated, i.e. there's no key with the same name as the placeholder. Maybe you meant to asign it a value but misspelled the key or placeholder name?" ]
        }
