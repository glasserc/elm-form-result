module Tests exposing (suite)

import Expect
import Form.Result.AnyJust as AnyJust
import Form.Result.Utils as Utils
import Test exposing (..)


suite : Test
suite =
    concat
        [ utilsTests
        , anyJustTests
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
