module Errors exposing (..)


type Errors
    = Errors (List String)


init : Errors
init =
    Errors []


add : String -> Errors -> Errors
add new (Errors mb) =
    new
        :: mb
        |> Errors


isEmpty : Errors -> Bool
isEmpty (Errors es) =
    List.isEmpty es


toStrings : Errors -> List String
toStrings (Errors es) =
    es
