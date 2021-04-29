module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "card parser"
        [ test "fails to parse money" <|
            \_ ->
                let
                    res =
                        Parser.run Main.cardsParser "   $ 30.20"
                in
                Expect.err res
        , test "can parse a card" <|
            \_ ->
                let
                    res =
                        Parser.run Main.cardsParser "   4x counterspell"
                in
                Expect.ok res
        ]
