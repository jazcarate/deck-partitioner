module MainTest exposing (..)

import Expect
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
        , test "ignores money after tab" <|
            \_ ->
                let
                    res =
                        Parser.run Main.cardsParser "4 \tFlooded Strand  \t$ 30.44"
                in
                Expect.equal res (Ok ( 4, "Flooded Strand" ))
        , test "can parse a card" <|
            \_ ->
                let
                    res =
                        Parser.run Main.cardsParser "   4x counterspell"
                in
                Expect.equal res (Ok ( 4, "counterspell" ))
        ]
