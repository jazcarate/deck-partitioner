module Main exposing (main)

import Html exposing (Html, text)
import List as L exposing (..)


type Tree a
    = One (List a)
    | Many (List (Tree a))


classifyOn : (a -> comparable) -> List a -> List (List a)
classifyOn field =
    let
        classification a b =
            field a == field b
    in
    classifyBy classification


classifyBy : (a -> a -> Bool) -> List a -> List (List a)
classifyBy eq l =
    case l of
        [] ->
            []

        x :: xs ->
            let
                neq a b =
                    not (eq a b)
            in
            (x :: L.filter (eq x) xs) :: classifyBy eq (L.filter (neq x) xs)


clasify : (a -> comparable) -> Tree a -> Tree a
clasify field tree =
    case tree of
        One leaves ->
            Many (L.map One (classifyOn field leaves))

        Many ts ->
            Many (L.map (clasify field) ts)


type alias Card =
    { color : String, cmc : String, name : String }


deck : List Card
deck =
    [ { color = "g", cmc = "1", name = "Arboreal Grazer" }
    , { color = "g", cmc = "1", name = "Arboreal Grazer" }
    , { color = "g", cmc = "3", name = "Dryad of the Ilysian Grove" }
    , { color = "g", cmc = "1", name = "Arboreal Grazer" }
    , { color = "g", cmc = "3", name = "Azusa, Lost but Seeking" }
    , { color = "g", cmc = "3", name = "Azusa, Lost but Seeking" }
    , { color = "g", cmc = "3", name = "Dryad of the Ilysian Grove" }
    , { color = "g", cmc = "6", name = "Primeval Titan" }
    , { color = "g", cmc = "3", name = "Dryad of the Ilysian Grove" }
    , { color = "g", cmc = "3", name = "Dryad of the Ilysian Grove" }
    , { color = "g", cmc = "3", name = "Tireless Tracker" }
    , { color = "g", cmc = "6", name = "Primeval Titan" }
    , { color = "g", cmc = "6", name = "Primeval Titan" }
    , { color = "g", cmc = "1", name = "Arboreal Grazer" }
    , { color = "g", cmc = "6", name = "Primeval Titan" }
    , { color = "g", cmc = "0", name = "Summoner's Pact" }
    , { color = "g", cmc = "0", name = "Summoner's Pact" }
    , { color = "g", cmc = "0", name = "Summoner's Pact" }
    , { color = "g", cmc = "0", name = "Summoner's Pact" }
    , { color = "c", cmc = "1", name = "Amulet of Vigor" }
    , { color = "c", cmc = "1", name = "Amulet of Vigor" }
    , { color = "c", cmc = "1", name = "Amulet of Vigor" }
    , { color = "c", cmc = "1", name = "Amulet of Vigor" }
    , { color = "c", cmc = "0", name = "Engineered Explosives" }
    , { color = "u", cmc = "0", name = "Pact of negation" }
    ]


unlines : List String -> String
unlines ls =
    foldr (\x y -> x ++ "\n" ++ y) "" ls


pre : List String -> List String
pre l =
    let
        prefix =
            if length l < 2 then
                singleton "──"

            else
                singleton "┌─" ++ repeat (length l - 2) "│ " ++ singleton "└─"
    in
    zipWith (++) prefix l


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f als bls =
    let
        go =
            zipWith f
    in
    case ( als, bls ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            f x y :: go xs ys


renderTree : (a -> String) -> Tree a -> List String
renderTree show tree =
    case tree of
        One leaves ->
            L.map show leaves
                |> pre

        Many ts ->
            intersperse (singleton "") (L.map (renderTree show) ts)
                |> concat
                |> pre


intersperse : a -> List a -> List a
intersperse sep ls =
    case ls of
        [] ->
            []

        x :: xs ->
            x :: prependToAll sep xs


prependToAll : a -> List a -> List a
prependToAll sep ls =
    case ls of
        [] ->
            []

        x :: xs ->
            sep :: x :: prependToAll sep xs


dt : Tree Card
dt =
    One deck
        |> clasify .cmc
        |> clasify .color


showCard : Card -> String
showCard =
    .name


main : Html msg
main =
    renderTree showCard dt
        |> unlines
        |> text
        |> singleton
        |> Html.pre []
