module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Errors exposing (Errors)
import Html exposing (..)
import Html.Attributes exposing (placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, string)
import List as L exposing (..)
import Parser exposing ((|.), (|=), Parser, number)
import Set


type Tree a
    = One (List a)
    | Many (List (Tree a))


classifyOn : Partitioner a -> List a -> List (List a)
classifyOn field =
    let
        classification a b =
            field.pfunc a == field.pfunc b
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


clasify : Partitioner a -> Tree a -> Tree a
clasify field tree =
    case tree of
        One leaves ->
            Many (L.map One (classifyOn field leaves))

        Many ts ->
            Many (L.map (clasify field) ts)


type alias Card =
    { color : String, cmc : String, name : String }


unlines : List String -> String
unlines ls =
    foldr (\x y -> x ++ "\n" ++ y) "" ls


pre : String -> List String -> List String
pre p l =
    let
        prefix =
            if length l < 2 then
                singleton ("─" ++ p ++ "─")

            else
                singleton ("┌" ++ p ++ "─") ++ repeat (length l - 2) ("│ " ++ String.repeat (String.length p) " ") ++ singleton ("└─" ++ String.repeat (String.length p) "─")
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
                |> pre (String.fromInt (cost_ tree))

        Many ts ->
            intersperse (singleton "") (L.map (renderTree show) ts)
                |> concat
                |> pre (String.fromInt (cost_ tree))


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


showCard : Card -> String
showCard =
    .name


deckExample : String
deckExample =
    unlines
        [ "4 Counterspell"
        , "20 Island"
        , "4 Ponder"
        , "4 Preordain"
        ]


view : Model -> Html Msg
view m =
    div []
        [ if Errors.isEmpty m.errors then
            span [] []

          else
            div []
                [ h3 [] [ text "errors" ]
                , code [] [ text <| unlines <| Errors.toStrings m.errors ]
                , hr [] []
                ]
        , viewInner m
        ]


viewInner : Model -> Html Msg
viewInner model =
    case model.status of
        Editting ->
            form [ onSubmit LoadCards ]
                [ textarea [ placeholder deckExample, value model.content, onInput Change ] []
                , button [ type_ "submit" ] [ text "Load" ]
                ]

        Showing ->
            let
                cards =
                    lookupCards model.db model.deck

                tree =
                    partitionBy model.partition cards
            in
            div []
                [ button [ onClick Edit ] [ text "<< Edit" ]
                , div []
                    [ span [] <| List.indexedMap viewPartition model.partition
                    , span [] [ text "by name" ]
                    ]
                , div [] [ button [ onClick (Partitions PAdd) ] [ text "add partition" ] ]
                , div [] [ button [ onClick FindBest ] [ text "Find best partition" ] ]
                , div [] [ text <| "Cost: " ++ String.fromInt (cost tree) ]
                , span [] <|
                    if List.length cards < List.length model.deck then
                        [ text <| "missing " ++ String.fromInt (List.length model.deck - List.length cards) ++ " cards" ]

                    else
                        [ text <| "complete!" ]
                , showTree tree
                ]


viewPartition : Int -> Partitioner Card -> Html Msg
viewPartition i p =
    span []
        [ Html.select
            []
            (List.map (\this -> viewOption i (p.pname == this.pname) this) partitions)
        , button [ onClick (Partitions (PRemove i)) ] [ text "🗑" ]
        ]


permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    List.map ((::) y) (permutations ys)
            in
            concatMap f (select xs)


subsequences : List a -> List (List a)
subsequences xs =
    [] :: subsequencesNonEmpty xs


subsequencesNonEmpty : List a -> List (List a)
subsequencesNonEmpty list =
    case list of
        [] ->
            []

        first :: rest ->
            let
                f ys r =
                    ys :: (first :: ys) :: r
            in
            [ first ] :: foldr f [] (subsequencesNonEmpty rest)


select : List a -> List ( a, List a )
select list =
    case list of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xs)


viewOption : Int -> Bool -> Partitioner Card -> Html Msg
viewOption i sel p =
    option
        [ onClick (Partitions (PChange i p)), selected sel ]
        [ text <| p.pname ]


partitionBy : List (Partitioner Card) -> List Card -> Tree Card
partitionBy pts cards =
    let
        allPts =
            List.append pts [ namePartition ]
    in
    List.foldl clasify (One cards) allPts


showTree : Tree Card -> Html msg
showTree partition =
    renderTree showCard partition
        |> unlines
        |> text
        |> singleton
        |> Html.pre []


type Status
    = Editting
    | Showing


type alias PartitionModel =
    List (Partitioner Card)


type alias Model =
    { content : String
    , errors : Errors
    , status : Status
    , db : Dict CardName Card
    , deck : List CardName
    , partition : PartitionModel
    }


type alias Comparable =
    String


type alias Partitioner a =
    { pfunc : a -> Comparable, pname : String }


type Msg
    = GotCard (Result Http.Error ( CardName, Card ))
    | Change String
    | LoadCards
    | Edit
    | FindBest
    | Partitions PartitionMsg


type PartitionMsg
    = PAdd
    | PRemove Int
    | PChange Int (Partitioner Card)


type alias MultiResult error value =
    { ok : value
    , err : error
    }


mapMR : (error1 -> error2) -> (value1 -> value2) -> MultiResult error1 value1 -> MultiResult error2 value2
mapMR fErr fVal mr =
    { ok = fVal mr.ok, err = fErr mr.err }


accumulate : List (Result e a) -> MultiResult (List e) (List a)
accumulate xs =
    { ok = List.filterMap Result.toMaybe xs
    , err = List.filterMap errToMaybe xs
    }


errToMaybe : Result e a -> Maybe e
errToMaybe mb =
    case mb of
        Err e ->
            Just e

        _ ->
            Nothing


type alias CardName =
    String


parseDeck : String -> MultiResult (List String) (List CardName)
parseDeck content =
    let
        cards =
            String.split "\n" content
                |> List.filter (not << String.isEmpty)
                |> List.map (Parser.run cardsParser)
                |> accumulate
    in
    mapMR (List.map deadEndsToString) multiply cards


multiply : List ( Int, a ) -> List a
multiply =
    uncurry List.repeat
        |> List.concatMap


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( a, b ) =
    f a b


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString =
    Debug.toString


httpErrorsToString : Http.Error -> String
httpErrorsToString =
    Debug.toString


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')


cardsParser : Parser ( Int, String )
cardsParser =
    Parser.succeed Tuple.pair
        |. whitespace
        |= Parser.oneOf
            [ Parser.int
            , Parser.succeed 1
            ]
        |. Parser.oneOf
            [ Parser.symbol "x"
            , Parser.succeed ()
            ]
        |. whitespace
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> c /= '$' || c /= '\t')
           )
        |. Parser.end


loadCard : CardName -> Cmd Msg
loadCard name =
    Http.get
        { url = "https://api.scryfall.com/cards/named?fuzzy=" ++ name
        , expect = Http.expectJson (GotCard << Result.map (Tuple.pair name)) cardDecoder
        }


cardDecoder : Decoder Card
cardDecoder =
    Json.Decode.map3 (\color_ cmc_ name_ -> { color = color_, cmc = cmc_, name = name_ })
        (Json.Decode.map (Maybe.withDefault "") <| Json.Decode.maybe <| Json.Decode.field "colors" colorsDecode)
        (Json.Decode.map String.fromInt <| Json.Decode.field "cmc" Json.Decode.int)
        (Json.Decode.field "name" string)


colorsDecode : Decoder String
colorsDecode =
    Json.Decode.list string
        |> Json.Decode.map (List.sort >> String.concat)


subtract : List comparable -> List comparable -> List comparable
subtract a b =
    List.filter (\x -> not <| List.member x b) a


defaultPartition : Partitioner Card
defaultPartition =
    { pfunc = .cmc, pname = "by mana value" }


namePartition : Partitioner Card
namePartition =
    { pfunc = .name, pname = "by name" }


partitions : List (Partitioner Card)
partitions =
    [ defaultPartition, { pfunc = .color, pname = "by color" } ]


updatePartition : PartitionMsg -> PartitionModel -> PartitionModel
updatePartition msg model =
    case msg of
        PAdd ->
            List.append model [ defaultPartition ]

        PRemove i ->
            removeAt i model

        PChange i new ->
            updateAt i (\_ -> new) model


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                head ++ fn x :: xs

            _ ->
                list


removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l

    else
        case drop index l of
            [] ->
                l

            _ :: rest ->
                take index l ++ rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.status ) of
        ( Change c, Editting ) ->
            ( { model | content = c }, Cmd.none )

        ( Edit, Showing ) ->
            ( { model | status = Editting }, Cmd.none )

        ( Partitions pmsg, Showing ) ->
            ( { model | partition = updatePartition pmsg model.partition }, Cmd.none )

        ( LoadCards, Editting ) ->
            let
                res =
                    parseDeck model.content

                errors =
                    if List.isEmpty res.err then
                        Errors.init

                    else
                        List.foldl Errors.add Errors.init res.err

                missingCards =
                    subtract res.ok (Dict.keys model.db)
            in
            ( { model
                | status = Showing
                , errors = errors
                , deck = res.ok
              }
            , Cmd.batch <| List.map loadCard <| unique missingCards
            )

        ( GotCard res, _ ) ->
            case res of
                Err e ->
                    ( { model | errors = Errors.add (httpErrorsToString e) model.errors }, Cmd.none )

                Ok ( name, card ) ->
                    ( { model | db = Dict.insert name card model.db }, Cmd.none )

        ( FindBest, _ ) ->
            ( { model | partition = findBestPartition model }, Cmd.none )

        _ ->
            ( { model | errors = Errors.add "Shoudnt have gootten a message in this state" model.errors }, Cmd.none )


cost_ : Tree a -> Int
cost_ t =
    case t of
        One _ ->
            children t

        Many ls ->
            List.sum (List.map children ls)
                * (List.length ls * 2)


cost : Tree a -> Int
cost t =
    fold cost_ t + depth t


fold : (Tree a -> Int) -> Tree a -> Int
fold f t =
    case t of
        One _ ->
            f t

        Many ls ->
            List.sum (List.map f ls) + (List.foldl (+) 0 <| List.map (fold f) ls)


sum : (a -> Int) -> Tree a -> Int
sum f t =
    case t of
        One ls ->
            List.sum <| List.map f ls

        Many ls ->
            List.sum (List.map (sum f) ls)


depth : Tree a -> Int
depth t =
    case t of
        One _ ->
            0

        Many ls ->
            1 + List.foldl max 0 (List.map depth ls)


children : Tree a -> Int
children t =
    case t of
        One ls ->
            List.length ls

        Many ls ->
            List.foldl (+) 0 (List.map children ls)


findBestPartition : Model -> List (Partitioner Card)
findBestPartition model =
    let
        cards =
            lookupCards model.db model.deck
    in
    (\x -> List.concatMap subsequences (permutations x)) partitions
        |> List.sortBy (\parts -> cost <| partitionBy parts cards)
        |> List.head
        |> Maybe.withDefault []


unique : List comparable -> List comparable
unique =
    Set.fromList >> Set.toList


lookupCards : Dict CardName Card -> List CardName -> List Card
lookupCards db cards =
    List.filterMap (\card -> Dict.get card db) cards


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = deckExample
      , errors = Errors.init
      , status = Editting
      , db = Dict.empty
      , deck = []
      , partition = []
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
