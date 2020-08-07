module NoMissingTemplateValueTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Expect exposing (Expectation)
import NoMissingTemplateValue exposing (rule)
import Review.Test exposing (ExpectedError, ReviewResult)
import Test exposing (Test, concat, describe, test)


all : Test
all =
    concat
        [ identifiesPlaceholderSyntax
        , namePaddingIsIgnored
        , spacesInNamesIsNotIgnored
        , multiplePlaceholdersAndValues
        , reportsErrors
        ]


identifiesPlaceholderSyntax : Test
identifiesPlaceholderSyntax =
    describe "Identifies placeholder syntax"
        [ passes "foo" []
        , passes "${}" [ ( "", "" ) ]
        , passes "${${}" [ ( "${", "" ) ]
        ]


namePaddingIsIgnored : Test
namePaddingIsIgnored =
    describe "Name padding is ignored"
        [ passes "${foo}" [ ( "foo", "bar" ) ]
        , passes "${ foo}" [ ( "foo", "bar" ) ]
        , passes "${foo }" [ ( "foo", "bar" ) ]
        , passes "${   foo   }" [ ( "foo", "bar" ) ]
        ]


spacesInNamesIsNotIgnored : Test
spacesInNamesIsNotIgnored =
    describe "Spaces in Names is not ignored"
        [ passes "${foo}" [ ( "foo", "bar" ) ]
        , fails "${f oo}"
            [ ( "foo", "bar" ) ]
            [ placeholderWithoutValueError "${f oo}"
            , unusedKeyError "\"foo\""
            ]
        , passes "${b ar}" [ ( "b ar", "bar" ) ]
        , fails "${b az}"
            [ ( "baz", "foo" ) ]
            [ placeholderWithoutValueError "${b az}"
            , unusedKeyError "\"baz\""
            ]
        ]


multiplePlaceholdersAndValues : Test
multiplePlaceholdersAndValues =
    describe "Multiple placeholders and values"
        [ passes "${}{}" [ ( "", "foo" ) ]
        , passes "${x} ${y} ${x}" [ ( "x", "foo" ), ( "y", "bar" ) ]
        , fails "${x} ${y}"
            [ ( "x", "foo" ) ]
            [ placeholderWithoutValueError "${y}" ]
        , fails "${x}"
            [ ( "x", "foo" ), ( "y", "bar" ) ]
            [ unusedKeyError "\"y\"" ]
        ]


reportsErrors : Test
reportsErrors =
    describe "Reports errors"
        [ describe "Placeholders without values"
            [ fails "${x}" [] [ placeholderWithoutValueError "${x}" ]
            , fails "${x}${y}${z}"
                []
                [ placeholderWithoutValueError "${x}"
                , placeholderWithoutValueError "${y}"
                , placeholderWithoutValueError "${z}"
                ]
            , fails "${x}\n${y}"
                []
                [ placeholderWithoutValueError "${x}"
                , placeholderWithoutValueError "${y}"
                ]
            ]
        , describe "Unused keys"
            [ fails "foo" [ ( "x", "y" ) ] [ unusedKeyError "\"x\"" ]
            ]
        , describe "Duplicate keys"
            [ fails "${x}"
                [ ( "x", "y" ), ( "x", "z" ), ( "x", "w" ) ]
                [ duplicateKeysError "\"x\""
                    { start = Location 3 48
                    , end = Location 3 51
                    }
                , duplicateKeysError "\"x\""
                    { start = Location 3 62
                    , end = Location 3 65
                    }
                ]
            ]
        ]


passes : String -> List ( String, String ) -> Test
passes template toInject =
    unitTest template
        template
        toInject
        Review.Test.expectNoErrors


fails : String -> List ( String, String ) -> List ExpectedError -> Test
fails template toInject errors =
    unitTest template
        template
        toInject
        (Review.Test.expectErrors errors)


unitTest : String -> String -> List ( String, String ) -> (ReviewResult -> Expectation) -> Test
unitTest desc template toInject expect =
    let
        toInjectStr : String
        toInjectStr =
            "[ "
                ++ (List.map (\( key, value ) -> "( \"" ++ key ++ "\", \"" ++ value ++ "\" )") toInject
                        |> String.join ", "
                   )
                ++ " ]"
    in
    describe desc
        [ test "Normal application" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template
bar = String.Template.inject """ ++ toInjectStr ++ "\"\"\"" ++ template ++ "\"\"\"")
                    |> Review.Test.run rule
                    |> expect
        , test "Normal application with single quote template" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template
bar = String.Template.inject """
                    ++ toInjectStr
                    ++ "\""
                    ++ String.replace "\n" "\\n" template
                    ++ "\""
                )
                    |> Review.Test.run rule
                    |> expect

        --        , test "Normal application with `exposing` import" <|
        --            \_ ->
        --                ("""module Foo exposing (bar)
        --import String.Template exposing (inject)
        --bar = inject \"\"\"""" ++ template ++ "\"\"\"  " ++ toInjectStr)
        --                    |> Review.Test.run rule
        --                    |> expect
        --        , test "Normal application with `as` import" <|
        --            \_ ->
        --                ("""module Foo exposing (bar)
        --import String.Template as T
        --bar = T.inject \"\"\"""" ++ template ++ "\"\"\"  " ++ toInjectStr)
        --                    |> Review.Test.run rule
        --                    |> expect
        --        , test "Right pipe appilcation" <|
        --            \_ ->
        --                ("""module Foo exposing (bar)
        --import String.Template
        --bar = \"\"\"""" ++ template ++ "\"\"\" |> String.Template.inject " ++ toInjectStr)
        --                    |> Review.Test.run rule
        --                    |> expect
        --        , test "Left pipe application" <|
        --            \_ ->
        --                ("""module Foo exposing (bar)
        --import String.Template
        --bar = String.Template.inject """ ++ toInjectStr ++ " <| \"\"\"" ++ template ++ "\"\"\"")
        --                    |> Review.Test.run rule
        --                    |> expect
        ]


duplicateKeysError : String -> Range -> ExpectedError
duplicateKeysError under range =
    Review.Test.error
        { message = "Duplicate keys."
        , details = [ "You already have a key with the same name. Rename this key to something eles" ]
        , under = under
        }
        |> Review.Test.atExactly range


unusedKeyError : String -> ExpectedError
unusedKeyError under =
    Review.Test.error
        { message = "Unused key."
        , details = [ "This keys is not being used anywhere in the template, maybe you meant to use it but misspelled the key or placeholder name?" ]
        , under = under
        }


placeholderWithoutValueError : String -> ExpectedError
placeholderWithoutValueError under =
    Review.Test.error
        { message = "Placeholder has no value"
        , details = [ "This placeholder has no value associated, i.e. there's no key with the same name as the placeholder. Maybe you meant to asign it a value but misspelled the key or placeholder name?" ]
        , under = under
        }
