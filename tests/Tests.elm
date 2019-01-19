module Tests exposing (suite)

import Expect
import Form.Result exposing (validated)
import Form.Result.AnyJust as AnyJust
import Form.Result.Utils as Utils
import Test exposing (..)


suite : Test
suite =
    concat
        [ utilsTests
        , anyJustTests
        , resultTests
        ]


utilsTests : Test
utilsTests =
    describe "Utils module"
        [ describe "errToMaybe"
            [ test "on Ok produces Nothing" <|
                \_ ->
                    Expect.equal Nothing (Utils.errToMaybe <| Ok True)
            , test "on Err produces Just" <|
                \_ ->
                    Expect.equal (Just 4) (Utils.errToMaybe <| Err 4)
            ]
        , describe "ifNothing"
            [ test "on Just produces Nothing" <|
                \_ ->
                    Expect.equal Nothing (Utils.ifNothing (Just 5) "hello")
            , test "on Nothing produces Just" <|
                \_ ->
                    Expect.equal (Just "hello") (Utils.ifNothing Nothing "hello")
            ]
        ]


anyJustTests : Test
anyJustTests =
    let
        triple a b c =
            ( a, b, c )
    in
    describe "AnyJust tests"
        [ test "starts as not having seen a Just" <|
            \_ ->
                AnyJust.start 1
                    |> AnyJust.finish
                    |> Expect.equal Nothing
        , test "when fed with Nothings, produces Nothing" <|
            \_ ->
                AnyJust.start triple
                    |> AnyJust.check Nothing
                    |> AnyJust.check Nothing
                    |> AnyJust.check Nothing
                    |> AnyJust.finish
                    |> Expect.equal Nothing
        , test "when fed with a Just, produces Just" <|
            \_ ->
                AnyJust.start triple
                    |> AnyJust.check (Just 1)
                    |> AnyJust.check Nothing
                    |> AnyJust.check Nothing
                    |> AnyJust.finish
                    |> Expect.equal (Just ( Just 1, Nothing, Nothing ))
        ]


type alias TestErrorType =
    { field1 : Maybe String
    , field2 : Maybe Int
    }


type alias TestOutputType =
    { output1 : Int
    , output2 : String
    }


resultTests : Test
resultTests =
    describe "Form.Result tests"
        [ test "starts as Ok" <|
            \_ ->
                Form.Result.start "fail" 2
                    |> Form.Result.toResult
                    |> Expect.equal (Ok 2)
        , test "when fed Oks, produces Ok" <|
            \_ ->
                Form.Result.start TestErrorType TestOutputType
                    |> Form.Result.validated (Ok 1)
                    |> Form.Result.validated (Ok "hi")
                    |> Form.Result.toResult
                    |> Expect.equal (Ok <| TestOutputType 1 "hi")
        , test "when fed an Err, produces Err" <|
            \_ ->
                Form.Result.start TestErrorType TestOutputType
                    |> Form.Result.validated (Err "bad")
                    |> Form.Result.validated (Ok "hi")
                    |> Form.Result.toResult
                    |> Expect.equal (Err <| TestErrorType (Just "bad") Nothing)
        , test "when fed an Err, collects all Errs" <|
            \_ ->
                Form.Result.start TestErrorType TestOutputType
                    |> Form.Result.validated (Err "bad")
                    |> Form.Result.validated (Err 4)
                    |> Form.Result.toResult
                    |> Expect.equal (Err <| TestErrorType (Just "bad") (Just 4))
        , test "can reorder fields" <|
            \_ ->
                Form.Result.start TestErrorType TestOutputType
                    |> Form.Result.maybeErr Nothing
                    |> Form.Result.validated (Ok 1)
                    |> Form.Result.maybeValid (Just "hi")
                    |> Form.Result.toResult
                    |> Expect.equal (Ok <| TestOutputType 1 "hi")
        , describe "maybeErr"
            [ test "when Nothing, form is OK" <|
                \_ ->
                    Form.Result.start TestErrorType identity
                        |> Form.Result.maybeErr Nothing
                        |> Form.Result.validated (Ok 1)
                        -- We don't fill out all the error fields here. If
                        -- the test fails, we'll get some Err (Maybe Int -> TestErrorType).
                        |> Form.Result.toResult
                        |> Expect.equal (Ok 1)
            , test "when Just something, form is Err" <|
                \_ ->
                    Form.Result.start identity identity
                        |> Form.Result.maybeErr (Just 1)
                        |> Form.Result.toResult
                        |> Expect.equal (Err (Just 1))
            ]
        , describe "checkErr behaves like maybeErr"
            [ test "when Ok something, form is OK" <|
                \_ ->
                    Form.Result.start TestErrorType identity
                        -- `Ok` contents are discarded
                        |> Form.Result.checkErr (Ok "aces!")
                        |> Form.Result.validated (Ok 1)
                        |> Form.Result.toResult
                        |> Expect.equal (Ok 1)
            , test "when Err something, form is Err" <|
                \_ ->
                    Form.Result.start identity identity
                        |> Form.Result.checkErr (Err 1)
                        |> Form.Result.toResult
                        |> Expect.equal (Err (Just 1))
            ]
        , describe "ifMissing"
            [ test "when Just something, form is OK" <|
                \_ ->
                    Form.Result.start TestErrorType identity
                        |> Form.Result.ifMissing "never" (Just 1)
                        |> Form.Result.toResult
                        |> Expect.equal (Ok 1)
            , test "when Nothing, form is Err" <|
                \_ ->
                    Form.Result.start identity identity
                        |> Form.Result.ifMissing 1 Nothing
                        |> Form.Result.toResult
                        |> Expect.equal (Err (Just 1))
            ]
        , test "unconditionalErr does not change state of form" <|
            \_ ->
                Form.Result.start TestErrorType identity
                    |> Form.Result.unconditionalErr (Just "hi")
                    |> Form.Result.unconditionalErr Nothing
                    |> Form.Result.maybeValid (Just 1)
                    |> Form.Result.toResult
                    |> Expect.equal (Ok 1)
        ]
