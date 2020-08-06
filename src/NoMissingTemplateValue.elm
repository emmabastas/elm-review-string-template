module NoMissingTemplateValue exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser, Step(..))
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
                Expression.Application [ Node _ (Expression.FunctionOrValue mn n), Node _ (Expression.ListExpr exprs), Node templateRange (Expression.Literal template) ] ->
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

                        actualTemplateRange =
                            { start =
                                Location
                                    templateRange.start.row
                                    (templateRange.start.column + 1)
                            , end =
                                Location
                                    templateRange.end.row
                                    (templateRange.end.column - 9)
                            }
                    in
                    if mn == moduleName && n == name && List.length exprs == List.length keys then
                        ( checkForErrors (Node templateRange template) keys, context )

                    else
                        ( [], context )

                _ ->
                    ( [], context )


checkForErrors : Node String -> List (Node String) -> List (Error {})
checkForErrors (Node templateRange template) keyNodes =
    case keyNodesToDict keyNodes of
        Ok dict ->
            case Parser.run (Parser.loop (Normal []) (parser templateRange)) template of
                Err deadEnds ->
                    [ failedToParseTemplateError deadEnds ]

                Ok placeholders ->
                    let
                        ( errors, unusedKeys ) =
                            List.foldl
                                (\placeholder ( errs, uk ) ->
                                    case Dict.get placeholder.name dict of
                                        Just _ ->
                                            ( errs
                                            , Dict.remove placeholder.name uk
                                            )

                                        Nothing ->
                                            ( placeholderWithoutValueError
                                                placeholder.range
                                                :: errs
                                            , uk
                                            )
                                )
                                ( [], dict )
                                placeholders

                        unusedKeyErrors =
                            unusedKeys
                                |> Dict.toList
                                |> List.map (\( _, range ) -> unusedKeyError range)
                    in
                    errors ++ unusedKeyErrors

        Err errors ->
            errors


keyNodesToDict : List (Node String) -> Result (List (Error {})) (Dict String Range)
keyNodesToDict keyNodes =
    let
        ( errors, dict ) =
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
    in
    if List.length errors == 0 then
        Ok dict

    else
        Err errors


type State
    = Normal Placeholders
    | InPlaceholder Placeholders


type alias Placeholders =
    List { range : Range, name : String }


parser : Range -> State -> Parser (Step State Placeholders)
parser templateRange state =
    case state of
        Normal placeholders ->
            Parser.oneOf
                [ Parser.chompUntil "${"
                    |> Parser.map (\_ -> Loop (InPlaceholder placeholders))
                , Parser.succeed (Done placeholders)
                ]

        InPlaceholder placeholders ->
            Parser.succeed
                (\r1 c1 o1 r2 c2 o2 s ->
                    let
                        range =
                            { start =
                                Location
                                    (r1 + templateRange.start.row - 1)
                                    (c1 + templateRange.start.column)
                            , end =
                                Location
                                    (r2 + templateRange.start.row - 1)
                                    (c2 + templateRange.start.column)
                            }

                        name =
                            String.slice o1 (o2 + 1) s
                                |> placeholderNameFromPlaceholder
                    in
                    Loop (Normal ({ range = range, name = name } :: placeholders))
                )
                |= Parser.getRow
                |= Parser.getCol
                |= Parser.getOffset
                |. Parser.chompUntil "}"
                |= Parser.getRow
                |= Parser.getCol
                |= Parser.getOffset
                |= Parser.getSource


failedToParseTemplateError : List Parser.DeadEnd -> Error {}
failedToParseTemplateError deadEnds =
    Rule.error
        { message = "Internal error"
        , details = [ "Hmmm... This is probably a bug, if you could just copy this error message and send it to @emmabastas in slack or github that would be highly appreciated! :)", Parser.deadEndsToString deadEnds ]
        }
        (case deadEnds of
            [] ->
                { start = Location 0 0
                , end = Location 0 1
                }

            { row, col } :: _ ->
                { start = Location row col
                , end = Location row (col + 1)
                }
        )


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



-- helpers


placeholderNameFromPlaceholder : String -> String
placeholderNameFromPlaceholder placeholder =
    placeholder
        |> String.dropLeft 2
        |> String.dropRight 1
        |> String.toList
        |> dropWhile ((==) ' ')
        |> dropWhileRight ((==) ' ')
        |> String.fromList



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
