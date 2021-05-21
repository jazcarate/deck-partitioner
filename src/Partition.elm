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


renderPartition : (a -> String) -> Partition a -> List String
renderPartition show tree =
    let
        go =
            renderPartition show

        meta =
            cost_ >> String.fromFloat
    in
    case tree of
        One leaves ->
            List.map show leaves
                |> pre

        Many ts ->
            List.intersperse [ "" ] (List.map go ts)
                |> List.concat
                |> pre


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


pre : List String -> List String
pre l =
    let
        prefix =
            if List.length l < 2 then
                [ "───" ]

            else
                ("┌──") :: List.repeat (List.length l - 2) ("│  ") ++ [ "└──" ]
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
