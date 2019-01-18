module Tests exposing (suite)

import Expect
import Form.Result.Utils as Utils
import Test exposing (..)


suite : Test
suite =
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
