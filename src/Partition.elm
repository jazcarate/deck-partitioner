module Partition exposing (..)

import Html exposing (Html, text)


type Partition a
    = One (List a)
    | Many (List (Partition a))


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
            (x :: List.filter (eq x) xs) :: classifyBy eq (List.filter (neq x) xs)


renderPartition : (a -> Html msg) -> Partition a -> List (Html msg)
renderPartition show tree =
    let
        go =
            renderPartition show
    in
    case tree of
        One leaves ->
            List.map show leaves
                |> pre (text "─")

        Many ts ->
            List.map go ts
                |> List.intersperse [ Html.text "" ]
                |> List.concat
                |> pre (text "1")


cost_ : Partition a -> Float
cost_ t =
    case t of
        One _ ->
            1

        Many ls ->
            toFloat (List.length ls) / 2


cost : Partition a -> Float
cost =
    fold (\c -> toFloat (children c) * cost_ c)


fold : (Partition a -> Float) -> Partition a -> Float
fold f t =
    case t of
        One _ ->
            f t

        Many ls ->
            f t + List.sum (List.map (fold f) ls)


children : Partition a -> Int
children t =
    case t of
        One ls ->
            List.length ls

        Many ls ->
            List.sum <| List.map children ls


pre : Html msg -> List (Html msg) -> List (Html msg)
pre p l =
    let
        prefix =
            if List.length l < 2 then
                [ text "───", p, text "─" ]

            else
                Html.span [] [ Html.text "┌", p, text "─" ] :: (List.map Html.text <| List.repeat (List.length l - 2) "│  ") ++ [ Html.text "└──" ]
    in
    zipWith (\a b -> Html.span [] [ a, b ]) prefix l


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
