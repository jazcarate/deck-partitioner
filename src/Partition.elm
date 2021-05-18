module Partition exposing (..)


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


renderPartition : (a -> String) -> (Partition a -> String) -> Partition a -> List String
renderPartition show meta tree =
    let
        go =
            renderPartition show meta
    in
    case tree of
        One leaves ->
            List.map show leaves
                |> pre (meta tree)

        Many ts ->
            List.intersperse [ "" ] (List.map go ts)
                |> List.concat
                |> pre (meta tree)


pre : String -> List String -> List String
pre p l =
    let
        prefix =
            if List.length l < 2 then
                [ "─" ++ p ++ "─" ]

            else
                ("┌" ++ p ++ "─") :: List.repeat (List.length l - 2) ("│ " ++ String.repeat (String.length p) " ") ++ [ "└─" ++ String.repeat (String.length p) "─" ]
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
