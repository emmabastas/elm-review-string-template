module NoMissingTemplateValueTest exposing (all)

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
        ]


identifiesPlaceholderSyntax : Test
identifiesPlaceholderSyntax =
    describe "Identifies placeholder syntax"
        [ passes "" []
        , passes "foo" []
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
            [ placeholderNotInterpolated "${f oo}"
            , keyWithoutPlaceholder "foo"
            ]
        , passes "${f oo}" [ ( "f oo", "bar" ) ]
        , fails "${f oo}"
            [ ( "foo", "bar" ) ]
            [ placeholderNotInterpolated "${foo}"
            , keyWithoutPlaceholder "foo"
            ]
        ]


multiplePlaceholdersAndValues : Test
multiplePlaceholdersAndValues =
    describe "Multiple placeholders and values"
        [ passes "${}{}" [ ( "", "foo" ) ]
        , passes "${x} ${y} ${x}" [ ( "x", "foo" ), ( "y", "bar" ) ]
        , fails "${x} ${y}"
            [ ( "x", "foo" ) ]
            [ placeholderNotInterpolated "y" ]
        , fails "${x}"
            [ ( "x", "foo" ), ( "y", "bar" ) ]
            [ keyWithoutPlaceholder "y" ]
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
                ++ (List.map (\( key, value ) -> "( " ++ key ++ ", " ++ value ++ ")") toInject
                        |> String.join ", "
                   )
                ++ " ]"
    in
    describe desc
        [ test "Normal application" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template
bar = String.Template.inject \"\"\"""" ++ template ++ "\"\"\"  " ++ toInjectStr)
                    |> Review.Test.run rule
                    |> expect
        , test "Normal application with `exposing` import" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template exposing (inject)
bar = inject \"\"\"""" ++ template ++ "\"\"\"  " ++ toInjectStr)
                    |> Review.Test.run rule
                    |> expect
        , test "Normal application with `as` import" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template as T
bar = T.inject \"\"\"""" ++ template ++ "\"\"\"  " ++ toInjectStr)
                    |> Review.Test.run rule
                    |> expect
        , test "Right pipe appilcation" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template
bar = \"\"\"""" ++ template ++ "\"\"\" |> String.Template.inject " ++ toInjectStr)
                    |> Review.Test.run rule
                    |> expect
        , test "Left pipe application" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template
bar = String.Template.inject """ ++ toInjectStr ++ " <| \"\"\"" ++ template ++ "\"\"\"")
                    |> Review.Test.run rule
                    |> expect
        ]


placeholderNotInterpolated : String -> ExpectedError
placeholderNotInterpolated under =
    Review.Test.error
        { message = "Placeholder has no matching value"
        , details =
            [ "This would result in the placeholder appearing in the final string."
            , "Make sure that there's a value for this placeholder and that the key matches the placeholder name. Remember that leading and trailing space in the name is ignored!"
            ]
        , under = under
        }


keyWithoutPlaceholder : String -> ExpectedError
keyWithoutPlaceholder under =
    Review.Test.error
        { message = "Value has no matching placeholder"
        , details =
            [ "Every value should be inserted somewhere, but this value is not."
            , "Make sure that there's a placeholder for this value and that the key matches the placeholder name. Remember that leading and trailing space in the name is ignored!"
            ]
        , under = under
        }
