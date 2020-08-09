module NoElmStringTemplateMisuseTest exposing (all)

import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import NoElmStringTemplateMisuse exposing (rule)
import Review.Test exposing (ExpectedError, ReviewResult)
import Test exposing (Test, concat, describe, fuzz, test)


all : Test
all =
    concat
        [ validStringTemplateUseUnitTests
        , validStringTemplateUseFuzzTest
        ]


validStringTemplateUseUnitTests : Test
validStringTemplateUseUnitTests =
    describe "Valid String.Template use"
        ([ { template = "${}"
           , toInject = [ ( "", "x" ) ]
           }
         , { template = "${foo}"
           , toInject = [ ( "foo", "bar" ) ]
           }
         , { template = "${ foo }"
           , toInject = [ ( " foo ", "bar" ) ]
           }
         , { template = "${${}"
           , toInject = [ ( "${", "foo" ) ]
           }
         , { template = "$${foo}}"
           , toInject = [ ( "foo", "bar" ) ]
           }
         , { template = "${identity}"
           , toInject = [ ( "identity", "${identity}" ) ]
           }
         , { template = "${\\\\}"
           , toInject = [ ( "\\\\", "" ) ]
           }
         ]
            |> List.map
                (\{ template, toInject } ->
                    unitTest template
                        template
                        toInject
                        Review.Test.expectNoErrors
                )
        )


validStringTemplateUseFuzzTest : Test
validStringTemplateUseFuzzTest =
    let
        fuzzer :
            Fuzzer
                (List
                    { placeholder : String
                    , name : String
                    , leadingText : String
                    , trailingText : String
                    }
                )
        fuzzer =
            Fuzz.list placeholderSuroundedByTextFuzzer
                |> Fuzz.map
                    (List.map
                        (\e -> { e | leadingText = "_" ++ e.leadingText })
                    )
    in
    fuzz fuzzer "Valid String.Template use fuzz test" <|
        \segments ->
            let
                segmentsPrefixed :
                    List
                        { name : String
                        , leadingText : String
                        , trailingText : String
                        }
                segmentsPrefixed =
                    List.indexedMap
                        (\i { name, leadingText, trailingText } ->
                            { name =
                                String.padLeft
                                    (List.length segments // 10 + 1)
                                    '0'
                                    (String.fromInt i)
                                    ++ name
                                    |> String.replace "\"" ""
                                    |> String.replace "\\" "\\\\"
                            , leadingText =
                                leadingText
                                    |> String.replace "\"" ""
                                    |> String.replace "\\" "\\\\"
                            , trailingText =
                                trailingText
                                    |> String.replace "\"" ""
                                    |> String.replace "\\" "\\\\"
                            }
                        )
                        segments

                template : String
                template =
                    List.map
                        (\{ name, leadingText, trailingText } ->
                            leadingText ++ "${" ++ name ++ "}" ++ trailingText
                        )
                        segmentsPrefixed
                        |> String.concat
                        |> String.replace "\"\"\"" " "

                toInject : String
                toInject =
                    "[ "
                        ++ (List.map
                                (\{ name } -> "( \"" ++ name ++ "\", \"\")")
                                segmentsPrefixed
                                |> String.join ", "
                           )
                        ++ " ]"
            in
            ("""module Foo exposing (bar)
import String.Template
bar = String.Template.inject """ ++ toInject ++ "\"\"\"" ++ template ++ "\"\"\"")
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors


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
        , test "Normal application with `exposing(inject)` import" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template exposing (inject)
bar = inject """ ++ toInjectStr ++ "\"\"\"" ++ template ++ "\"\"\"")
                    |> Review.Test.run rule
                    |> expect
        , test "Normal application with `exposing(..)` import" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template exposing (..)
bar = inject """ ++ toInjectStr ++ "\"\"\"" ++ template ++ "\"\"\"")
                    |> Review.Test.run rule
                    |> expect
        , test "Normal application with `as` import" <|
            \_ ->
                ("""module Foo exposing (bar)
import String.Template as T
bar = T.inject """ ++ toInjectStr ++ "\"\"\"" ++ template ++ "\"\"\"")
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



-- Error helpers


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



-- Fuzzers


placeholderSuroundedByTextFuzzer :
    Fuzzer
        { placeholder : String
        , name : String
        , leadingText : String
        , trailingText : String
        }
placeholderSuroundedByTextFuzzer =
    Fuzz.map3
        (\{ placeholder, name } leadingText trailingText ->
            { placeholder = placeholder
            , name = name
            , leadingText = leadingText
            , trailingText = trailingText
            }
        )
        placeholderFuzzer
        textFuzzer
        textFuzzer


placeholderFuzzer : Fuzzer { placeholder : String, name : String }
placeholderFuzzer =
    Fuzz.map
        (\name -> { placeholder = "${" ++ name ++ "}", name = name })
        placeholderNameFuzzer


placeholderNameFuzzer : Fuzzer String
placeholderNameFuzzer =
    Fuzz.string
        |> Fuzz.map (String.replace "}" "")


textFuzzer : Fuzzer String
textFuzzer =
    Fuzz.string
        |> Fuzz.map (String.replace "${" "")
