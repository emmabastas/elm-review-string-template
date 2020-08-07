module NoMissingTemplateValue exposing (rule)

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Regex exposing (Regex)
import Review.Rule as Rule exposing (Error, Rule)


type alias Context =
    { injectName : Maybe ( List String, String )
    }


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTemplateValue" { injectName = Nothing }
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor node context =
    let
        moduleName : List String
        moduleName =
            Node.value (Node.value node).moduleName

        exposingList : Maybe Exposing.Exposing
        exposingList =
            Maybe.map Node.value (Node.value node).exposingList
    in
    case ( moduleName, exposingList ) of
        ( [ "String", "Template" ], Nothing ) ->
            ( [], { injectName = Just ( [ "String", "Template" ], "inject" ) } )

        ( [ "String", "Template" ], Just (Exposing.All _) ) ->
            ( [], { injectName = Just ( [], "inject" ) } )

        ( [ "String", "Template" ], Just (Exposing.Explicit exposedFunctions) ) ->
            let
                isInjectFunction : Node Exposing.TopLevelExpose -> Bool
                isInjectFunction exposeNode =
                    case Node.value exposeNode of
                        Exposing.FunctionExpose "inject" ->
                            True

                        _ ->
                            False
            in
            if List.any isInjectFunction exposedFunctions then
                ( [], { injectName = Just ( [], "inject" ) } )

            else
                ( [], context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case context.injectName of
        Nothing ->
            ( [], context )

        Just ( moduleName, name ) ->
            case Node.value node of
                Expression.Application [ Node _ (Expression.FunctionOrValue mn n), Node _ (Expression.ListExpr exprs), Node literalRange (Expression.Literal template) ] ->
                    let
                        keys : List (Node String)
                        keys =
                            List.filterMap
                                (\expr ->
                                    case Node.value expr of
                                        Expression.TupledExpression [ Node range (Expression.Literal key), _ ] ->
                                            Just (Node range key)

                                        _ ->
                                            Nothing
                                )
                                exprs

                        templateRange : Range
                        templateRange =
                            stringRangeInLiteral (Node literalRange template)
                    in
                    if
                        mn
                            == moduleName
                            && n
                            == name
                            && List.length exprs
                            == List.length keys
                    then
                        ( checkForErrors (Node templateRange template) keys
                        , context
                        )

                    else
                        ( [], context )

                _ ->
                    ( [], context )


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

        templateCursorPosition : Array ( Int, Int )
        templateCursorPosition =
            List.foldl
                (\char ( ( c, r ), xs ) ->
                    case char of
                        '\n' ->
                            ( ( 1, r + 1 ), ( 1, r + 1 ) :: xs )

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
                        ( oc, or ) =
                            ( (Node.range template).start.column
                            , (Node.range template).start.row
                            )

                        ( sc, sr ) =
                            Array.get startIndex templateCursorPosition
                                |> Maybe.withDefault ( 0, 0 )

                        ( ec, er ) =
                            Array.get (endIndex - 1) templateCursorPosition
                                |> Maybe.withDefault ( 0, 0 )

                        start =
                            if sr == 1 then
                                Location or (oc + sc - 1)

                            else
                                Location (or + sr - 1) (sc - 1)

                        end =
                            if er == 1 then
                                Location or (oc + ec)

                            else
                                Location (or + er - 1) ec
                    in
                    placeholderWithoutValueError
                        { start = start
                        , end = end
                        }
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
        |> String.toList
        |> dropWhile ((==) ' ')
        |> dropWhileRight ((==) ' ')
        |> String.fromList


stringRangeInLiteral : Node String -> Range
stringRangeInLiteral (Node range string) =
    if range.start.row == range.end.row then
        let
            literalLength =
                range.end.column - range.start.column

            stringLength =
                String.length string

            delta =
                literalLength - stringLength
        in
        { start =
            { row = range.start.row
            , column = range.start.column + (delta // 2)
            }
        , end =
            { row = range.end.row
            , column = range.end.column - (delta // 2)
            }
        }

    else
        Debug.todo "Handle multiline strings"


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
        , column = column + start.column - 1
        }
    , end =
        { row = row + end.row - 1
        , column =
            if end.row == 1 then
                column + end.column - 1

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



-- dropwWhile and dropWhileRight taken from elm-community/list-extra


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list


dropWhileRight : (a -> Bool) -> List a -> List a
dropWhileRight p =
    List.foldr
        (\x xs ->
            if p x && List.isEmpty xs then
                []

            else
                x :: xs
        )
        []
