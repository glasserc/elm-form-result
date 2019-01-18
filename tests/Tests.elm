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
                    |> Form.Result.andErr Nothing
                    |> Form.Result.validated (Ok 1)
                    |> Form.Result.maybeValid (Just "hi")
                    |> Form.Result.toResult
                    |> Expect.equal (Ok <| TestOutputType 1 "hi")
        ]
